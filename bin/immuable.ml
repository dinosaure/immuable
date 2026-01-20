let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let errorf fmt = Fmt.kstr (fun msg -> Error msg) fmt
let ( let@ ) finally fn = Fun.protect ~finally fn
let () = Logs_threaded.enable ()

let entry_of_filename ?(push = ignore) ?tbl alg filename =
  assert (Fpath.is_dir_path filename = false);
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let s = Unix.stat (Fpath.to_string filename) in
  match s.Unix.st_kind with
  | Unix.S_REG ->
      let open Unix in
      let len = s.Unix.st_size in
      let hdr = Fmt.str "blob %d\000" len in
      let ctx = Hash.empty in
      let ctx = Hash.feed_string ctx hdr in
      let fd = openfile (Fpath.to_string filename) [ O_RDONLY ] 0o644 in
      let finally () = Unix.close fd in
      Fun.protect ~finally @@ fun () ->
      let barr = map_file fd Bigarray.char Bigarray.c_layout false [| len |] in
      let bstr = Bigarray.array1_of_genarray barr in
      push (filename, bstr);
      let ctx = Hash.feed_bigstring ctx bstr in
      let uid = Hash.(to_raw_string (get ctx)) in
      let uid = Carton.Uid.unsafe_of_string uid in
      Option.iter (fun tbl -> Hashtbl.add tbl uid (`File (filename, len))) tbl;
      Cartonnage.Entry.make ~kind:`C ~length:len uid ()
  | Unix.S_DIR -> assert false
  | Unix.S_CHR | S_BLK | S_FIFO | S_SOCK ->
      Fmt.failwith "Impossible to save %a" Fpath.pp filename
  | Unix.S_LNK -> assert false (* TODO *)

module Tree = Immuable_tree

let entry_of_tree ?tbl alg tree =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let ref_length = Hash.digest_size in
  let str = Tree.to_string ~ref_length tree in
  let hdr = Fmt.str "tree %d\000" (String.length str) in
  let ctx = Hash.empty in
  let ctx = Hash.feed_string ctx hdr in
  let ctx = Hash.feed_string ctx str in
  let uid = Hash.get ctx |> Hash.to_raw_string |> Carton.Uid.unsafe_of_string in
  Option.iter (fun tbl -> Hashtbl.add tbl uid (`Tree str)) tbl;
  Cartonnage.Entry.make ~kind:`B ~length:(String.length str) uid ()

let entry_of_metadata ?tbl alg mtbl =
  let seq_of_metadata mtbl =
    let seq = Hashtbl.to_seq mtbl in
    let fn (filepath, mime) = Fmt.str "%a\000%s\000" Fpath.pp filepath mime in
    Seq.map fn seq
  in
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let len =
    Seq.fold_left
      (fun len str -> String.length str + len)
      0 (seq_of_metadata mtbl)
  in
  let bstr = Bstr.create len in
  let fn dst_off str =
    let len = String.length str in
    Bstr.blit_from_string str ~src_off:0 bstr ~dst_off ~len;
    dst_off + len
  in
  let _len = Seq.fold_left fn 0 (seq_of_metadata mtbl) in
  let hdr = Fmt.str "mime %d\000" len in
  let ctx = Hash.empty in
  let ctx = Hash.feed_string ctx hdr in
  let ctx = Hash.feed_bigstring ctx bstr in
  let uid = Hash.get ctx |> Hash.to_raw_string |> Carton.Uid.unsafe_of_string in
  Option.iter (fun tbl -> Hashtbl.add tbl uid (`Mime bstr)) tbl;
  Cartonnage.Entry.make ~kind:`D ~length:len uid ()

let entry_of_commit ?tbl alg ~(root : Carton.Uid.t) ~(metadata : Carton.Uid.t) =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let str = Fmt.str "%s%s" (root :> string) (metadata :> string) in
  let hdr = Fmt.str "commit %d\000" (String.length str) in
  let ctx = Hash.empty in
  let ctx = Hash.feed_string ctx hdr in
  let ctx = Hash.feed_string ctx str in
  let uid = Hash.get ctx |> Hash.to_raw_string |> Carton.Uid.unsafe_of_string in
  Option.iter (fun tbl -> Hashtbl.add tbl uid (`Commit str)) tbl;
  Cartonnage.Entry.make ~kind:`A ~length:(String.length str) uid ()

let walk_on_files alg (push, rtbl, tbl) path entries =
  let entry = entry_of_filename ?push ?tbl alg path in
  Logs.debug (fun m -> m "[+] %a" Fpath.pp path);
  Hashtbl.add rtbl path (Cartonnage.Entry.uid entry);
  entry :: entries

let rec walk_on_directories alg (rtbl, tbl) path entries =
  let lst = Bos.OS.Dir.contents ~rel:true path in
  let lst = Result.value ~default:[] lst in
  let lst = List.sort Fpath.compare lst in
  let ( let* ) = Option.bind in
  let fn0 rel =
    let abs = Fpath.(path // rel) in
    let stat = Unix.stat (Fpath.to_string abs) in
    let* perm =
      match stat.Unix.st_kind with
      | Unix.S_REG when stat.Unix.st_perm = 0o755 -> Some `Exec
      | Unix.S_REG when stat.Unix.st_perm = 0o664 -> Some `Everybody
      | Unix.S_REG -> Some `Normal
      | Unix.S_DIR -> Some `Dir
      | Unix.S_LNK -> Some `Link
      | _ -> None
    in
    Logs.debug (fun m -> m "search %a" Fpath.pp abs);
    let* node = Hashtbl.find_opt rtbl abs in
    Some { Tree.perm; name= Fpath.basename rel; node }
  in
  let fn1 = walk_on_directories alg (rtbl, tbl) in
  let abss = List.map (fun rel -> Fpath.(path // rel)) lst in
  let entries =
    Bos.OS.Path.fold ~traverse:`None ~elements:`Dirs fn1 entries abss
  in
  let entries = Result.value ~default:[] entries in
  let tree = List.filter_map fn0 lst in
  let entry = entry_of_tree ?tbl alg tree in
  Logs.debug (fun m -> m "[+] %a" Fpath.pp path);
  Hashtbl.add rtbl path (Cartonnage.Entry.uid entry);
  entry :: entries

let null alg =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  String.make Hash.digest_size '\000' |> Carton.Uid.unsafe_of_string

module Magic = struct
  open Conan.Sigs

  [@@@ocamlformat "disable"]

  module Make (S : sig type +'a t end) = struct type t
    external prj : ('a, t) io -> 'a S.t = "%identity"
    external inj : 'a S.t -> ('a, t) io = "%identity"
  end

  module Unix_scheduler = Make (struct type +'a t = 'a end)

  let unix = Unix_scheduler.{ bind= (fun x fn -> fn (prj x)); return= (fun x -> inj x ) }

  [@@@ocamlformat "enable"]

  module Access = struct
    type t = { bstr: Bstr.t; cache: Bstr.t Cachet.t; mutable seek: int }

    let make bstr =
      let map bstr ~pos len =
        if pos + len > Bstr.length bstr then begin
          let dst = Bstr.make len '\000' in
          let len = Int.min (Bstr.length bstr - pos) len in
          Bstr.blit bstr ~src_off:pos dst ~dst_off:0 ~len;
          dst
        end
        else Bstr.sub bstr ~off:pos ~len
      in
      let cache = Cachet.make ~map bstr in
      { bstr; cache; seek= 0 }

    let seek t offset value =
      let offset = Int64.to_int offset in
      match value with
      | Conan.Sigs.SET ->
          if offset < Bstr.length t.bstr then Ok (t.seek <- offset)
          else Error `Out_of_bound
      | Conan.Sigs.CUR ->
          if t.seek + offset < Bstr.length t.bstr then
            Ok (t.seek <- t.seek + offset)
          else Error `Out_of_bound
      | Conan.Sigs.END ->
          if Bstr.length t.bstr - offset > 0 then
            Ok (t.seek <- Bstr.length t.bstr - offset)
          else Error `Out_of_bound

    let read t len =
      let buf = Bytes.create len in
      try
        Cachet.blit_to_bytes t.cache ~src_off:t.seek buf ~dst_off:0 ~len;
        Ok (Bytes.unsafe_to_string buf)
      with _exn -> Error `Out_of_bound

    let read_int8 t =
      try Ok (Cachet.get_uint8 t.cache t.seek) with _ -> Error `Out_of_bound

    let read_int16_ne t =
      try Ok (Cachet.get_uint16_ne t.cache t.seek)
      with _ -> Error `Out_of_bound

    let read_int32_ne t =
      try Ok (Cachet.get_int32_ne t.cache t.seek)
      with _ -> Error `Out_of_bound

    let read_int64_ne t =
      try Ok (Cachet.get_int64_ne t.cache t.seek)
      with _ -> Error `Out_of_bound

    let line t =
      let buf = Buffer.create 0x7ff in
      let tmp = Bytes.create 0x7ff in
      let rec go src_off =
        let len = Int.min 0x7ff (Bstr.length t.bstr - src_off) in
        if src_off >= Bstr.length t.bstr || len <= 0 then Error `Out_of_bound
        else begin
          Cachet.blit_to_bytes t.cache ~src_off tmp ~dst_off:0 ~len;
          let str = Bytes.sub_string tmp 0 len in
          match String.index str '\n' with
          | pos ->
              Buffer.add_substring buf str 0 (pos + 1);
              t.seek <- src_off;
              let res = Buffer.contents buf in
              Ok (0, String.length res, res)
          | exception Not_found -> go (src_off + len)
        end
      in
      go t.seek
  end

  [@@@ocamlformat "disable"]

  let syscall =
    let open Unix_scheduler in
    let open Access in
    { seek= (fun f p w -> inj (seek f p w))
    ; read= (fun f l -> inj (read f l))
    ; read_int8= (fun f -> inj (read_int8 f))
    ; read_int16_ne= (fun f -> inj (read_int16_ne f))
    ; read_int32_ne= (fun f -> inj (read_int32_ne f))
    ; read_int64_ne= (fun f -> inj (read_int64_ne f))
    ; line= (fun f -> inj (line f)) }

  [@@@ocamlformat "enable"]

  let db =
    let tree = Conan_light.tree in
    Conan.Process.database ~tree

  let run bstr =
    let t = Access.make bstr in
    Conan.Process.descending_walk unix syscall t db
    |> Unix_scheduler.prj
    |> Conan.Metadata.mime
end

let rec clean rep orphans mtbl =
  match Miou.care orphans with
  | Some None | None -> ()
  | Some (Some prm) ->
      let filename, mime = Miou.await_exn prm in
      rep (1, filename);
      Option.iter (Hashtbl.add mtbl filename) mime;
      clean rep orphans mtbl

let rec terminate rep orphans mtbl =
  match Miou.care orphans with
  | None -> ()
  | Some None -> Miou.yield (); terminate rep orphans mtbl
  | Some (Some prm) ->
      let filename, mime = Miou.await_exn prm in
      rep (1, filename);
      Option.iter (Hashtbl.add mtbl filename) mime;
      terminate rep orphans mtbl

let rec tag rep orphans mtbl queue =
  match Flux.Bqueue.get queue with
  | Some (filename, bstr) ->
      clean rep orphans mtbl;
      let _ =
        Miou.call ~orphans @@ fun () ->
        let mime = Magic.run bstr in
        let pp ppf = function
          | None -> Fmt.string ppf "<none>"
          | Some mime -> Fmt.string ppf mime
        in
        Logs.debug (fun m -> m "[!] %a: %a" Fpath.pp filename pp mime);
        (filename, mime)
      in
      tag rep orphans mtbl queue
  | None -> terminate rep orphans mtbl

let value_of_filename ~len filename =
  let fd = Unix.openfile (Fpath.to_string filename) Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let gstr = Unix.map_file fd Bigarray.char Bigarray.c_layout false [| len |] in
  let bstr = Bigarray.array1_of_genarray gstr in
  Carton.Value.make ~kind:`C bstr

let signature alg =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let feed_bigstring bstr ctx = Hash.feed_bigstring ctx bstr in
  let feed_bytes buf ~off ~len ctx = Hash.feed_bytes ctx ~off ~len buf in
  let hash =
    {
      Carton.First_pass.feed_bytes
    ; feed_bigstring
    ; serialize= Fun.compose Hash.to_raw_string Hash.get
    ; length= Hash.digest_size
    }
  in
  Carton.First_pass.Digest (hash, Hash.empty)

let rename src dst =
  match Unix.rename src dst with
  | exception Unix.Unix_error (Unix.EXDEV, _, _) ->
      let ic = open_in_bin src in
      let oc = open_out_bin dst in
      let@ () = fun () -> close_in ic in
      let@ () = fun () -> close_out oc in
      let tmp = Bytes.create 65536 in
      let rec go () =
        let len = input ic tmp 0 (Bytes.length tmp) in
        if len > 0 then
          go (output_substring oc (Bytes.unsafe_to_string tmp) 0 len)
      in
      go ()
  | () -> ()

let output_pack alg result pagesize seq =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let filepath, oc =
    match result with
    | Some filepath ->
        let filepath = Fpath.to_string filepath in
        (filepath, open_out_bin filepath)
    | None -> Filename.open_temp_file "pack-" ".pack"
  in
  let signature =
    let@ () = fun () -> close_out oc in
    Seq.iter (output_string oc) seq;
    flush oc;
    let pos = pos_out oc in
    let signature =
      let ic = open_in_bin filepath in
      let@ () = fun () -> close_in ic in
      seek_in ic (pos - Hash.digest_size);
      let tmp = Bytes.create Hash.digest_size in
      really_input ic tmp 0 Hash.digest_size;
      Bytes.unsafe_to_string tmp |> Ohex.encode
    in
    if pos mod pagesize <> 0 then
      let len = (((pos / pagesize) + 1) * pagesize) - pos in
      output_string oc (String.make len '\000')
    else flush oc;
    signature
  in
  match result with
  | Some _ -> Ok ()
  | None ->
      let target = Fmt.str "pack-%s.pack" signature in
      rename filepath target; Ok ()

let pack_of_filename alg filename result pagesize =
  let tbl = Hashtbl.create 10 in
  (* let queue = Flux.Bqueue.(create with_close 0x7ff) in
  let prm = Miou.call @@ fun () -> tag ignore (Miou.orphans ()) mtbl queue in *)
  let entry2 = entry_of_filename ~tbl alg filename in
  (* let () = Flux.Bqueue.close queue in
  let () = Miou.await_exn prm in *)
  let node = Cartonnage.Entry.uid entry2 in
  let tree = [ { Tree.perm= `Normal; name= Fpath.filename filename; node } ] in
  let entry1 = entry_of_tree ~tbl alg tree in
  let metadata = null alg in
  let root = Cartonnage.Entry.uid entry1 in
  let entry0 = entry_of_commit ~tbl alg ~root ~metadata in
  let entries = [ entry0; entry1; entry2 ] in
  let targets = List.map Cartonnage.Target.make entries in
  let t = List.to_seq targets in
  let with_header = 3 in
  let with_signature = signature alg in
  let load uid () =
    match Hashtbl.find tbl uid with
    | `Commit str -> Carton.Value.of_string ~kind:`A str
    | `Tree str -> Carton.Value.of_string ~kind:`B str
    | `File (filename, len) -> value_of_filename ~len filename
  in
  let seq = Carton_miou_unix.to_pack ~with_header ~with_signature ~load t in
  output_pack alg result pagesize seq

let spinner =
  let open Progress.Line in
  let frames = [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ] in
  let spin = spinner ~frames () in
  let sep = spacer 1 in
  list [ spin; pair ~sep (sum ~width:10 ()) (using Fpath.to_string string) ]

let bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  let width = `Fixed 30 in
  list [ bar ~style ~width total; count_to total ]

let with_reporter ~config quiet t fn =
  if quiet then fn ignore else Progress.with_reporter ~config t fn

let pack_of_directory quiet (progress : Progress.Config.t) without_progress
    without_recognition alg root result pagesize =
  let tbl = Hashtbl.create 0x100
  and rtbl = Hashtbl.create 0x100
  and mtbl = Hashtbl.create 0x100 in
  let ( let* ) = Result.bind in
  let* entries =
    if without_recognition then
      let fn0 = walk_on_files alg (None, rtbl, Some tbl) in
      Bos.OS.Path.fold ~elements:`Files fn0 [] [ root ]
    else
      let queue = Flux.Bqueue.(create with_close 0x7ff) in
      with_reporter ~config:progress (quiet || without_progress) spinner
      @@ fun rep ->
      let prm = Miou.call @@ fun () -> tag rep (Miou.orphans ()) mtbl queue in
      let push (filepath, bstr) = Flux.Bqueue.put queue (filepath, bstr) in
      let fn0 = walk_on_files alg (Some push, rtbl, Some tbl) in
      let* entries = Bos.OS.Path.fold ~elements:`Files fn0 [] [ root ] in
      let () = Flux.Bqueue.close queue in
      let () = Miou.await_exn prm in
      Ok entries
  in
  let fn1 = walk_on_directories alg (rtbl, Some tbl) in
  let elements = `Dirs and traverse = `None in
  let* entries = Bos.OS.Path.fold ~elements ~traverse fn1 entries [ root ] in
  let root = Hashtbl.find_opt rtbl root in
  let* root = Option.to_result ~none:`Root_not_found root in
  let metadata = null alg in
  let entry1 = entry_of_metadata ~tbl alg mtbl in
  let entry0 = entry_of_commit ~tbl alg ~root ~metadata in
  let entries = entry0 :: entry1 :: entries in
  let load uid () =
    match Hashtbl.find tbl uid with
    | `Commit str -> Carton.Value.of_string ~kind:`A str
    | `Tree str -> Carton.Value.of_string ~kind:`B str
    | `File (filename, len) -> value_of_filename ~len filename
    | `Mime bstr -> Carton.Value.make ~kind:`D bstr
  in
  let with_header = List.length entries in
  let entries = List.to_seq entries in
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let ref_length = Hash.digest_size in
  let seq = Carton_miou_unix.delta ~ref_length ~load entries in
  let with_signature = signature alg in
  with_reporter ~config:progress
    (quiet || without_progress)
    (bar ~total:with_header)
  @@ fun rep ->
  let seq = Seq.map (fun entry -> rep 1; entry) seq in
  let seq = Carton_miou_unix.to_pack ~with_header ~with_signature ~load seq in
  output_pack alg result pagesize seq

let run quiet progress without_progress without_recognition alg root pagesize
    result =
  if Fpath.is_dir_path root then
    pack_of_directory quiet progress without_progress without_recognition alg
      root result pagesize
  else pack_of_filename alg root result pagesize

let run_mime quiet filename =
  let fd = Unix.openfile (Fpath.to_string filename) [ O_RDONLY ] 0o644 in
  let len = Unix.(fstat fd).Unix.st_size in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let barr = Unix.map_file fd Bigarray.char Bigarray.c_layout false [| len |] in
  let bstr = Bigarray.array1_of_genarray barr in
  let mime = Magic.run bstr in
  Logs.debug (fun m ->
      m "[!] %a: %a" Fpath.pp filename Fmt.(Dump.option string) mime);
  let to_show = Option.value ~default:"application/octet-stream" mime in
  if not quiet then Ok (Fmt.pr "%s\n%!" to_show)
  else
    match mime with
    | Some _ -> Ok ()
    | None -> errorf "Unrecognized file %a" Fpath.pp filename

let pp_error ppf = function
  | `Root_not_found -> Fmt.string ppf "Root not found"
  | `Msg msg -> Fmt.string ppf msg

let run quiet progress without_progress without_recognition alg root pagesize
    result =
  Miou_unix.run @@ fun () ->
  run quiet progress without_progress without_recognition alg root pagesize
    result
  |> Result.map_error (Fmt.str "%a." pp_error)

open Cmdliner

let output_options = "OUTPUT OPTIONS"

let verbosity =
  let env = Cmd.Env.info "IMMUABLE_LOGS" in
  Logs_cli.level ~docs:output_options ~env ()

let renderer =
  let env = Cmd.Env.info "IMMUABLE_FMT" in
  Fmt_cli.style_renderer ~docs:output_options ~env ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  let env = Cmd.Env.info "IMMUABLE_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr);
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let alg =
  let parser str =
    match String.lowercase_ascii str with
    | "sha1" -> Ok `SHA1
    | "sha256" -> Ok `SHA256
    | "sha512" -> Ok `SHA512
    | "sha3-256" -> Ok `SHA3_256
    | "sha3-512" -> Ok `SHA3_512
    | "blake2b" -> Ok `BLAKE2B
    | _ -> error_msgf "Invalid hash algorithm: %S" str
  in
  let pp ppf = function
    | `SHA1 -> Fmt.string ppf "sha1"
    | `SHA256 -> Fmt.string ppf "sha256"
    | `SHA512 -> Fmt.string ppf "sha512"
    | `SHA3_256 -> Fmt.string ppf "sha3-256"
    | `SHA3_512 -> Fmt.string ppf "sha3-512"
    | `BLAKE2B -> Fmt.string ppf "blake2b"
  in
  Arg.conv (parser, pp)

let algorithm =
  let doc = "The hash algorithm used to identify immuable entries." in
  let open Arg in
  value & opt alg `SHA1 & info [ "a"; "algorithm" ] ~doc ~docv:"HASH"

let root =
  let doc = "The file or the root directory to save into an immuable image." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str && Sys.is_directory str ->
        Ok (Fpath.to_dir_path v)
    | Ok _ as value when Sys.file_exists str -> value
    | Ok v -> error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let root = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  required & pos 0 (some root) None & info [] ~doc ~docv:"ROOT"

let pot x = x land (x - 1) == 0 && x != 0

let pagesize =
  let doc =
    "The memory page size specifed by the immuable unikernel (must be a power \
     of two)."
  in
  let parser str =
    try
      let n = int_of_string str in
      if not (pot n) then
        error_msgf "The memory page size must be a power of two."
      else Ok n
    with _ -> error_msgf "Invalid memory page size."
  in
  let pagesize = Arg.conv (parser, Fmt.int) in
  let open Arg in
  value & opt pagesize 512 & info [ "pagesize" ] ~doc ~docv:"BYTES"

let output =
  let doc = "The output immuable image." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str -> error_msgf "%a already exist" Fpath.pp v
    | Ok _ as value -> value
    | Error _ as err -> err
  in
  let output = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  value & opt (some output) None & info [ "o"; "output" ] ~doc ~docv:"OUTPUT"

let setup_progress max_width = Progress.Config.v ~max_width ()

let width =
  let doc = "Width of the terminal." in
  let default = Terminal.Size.get_columns () in
  let open Arg in
  value & opt (some int) default & info [ "width" ] ~doc ~docv:"WIDTH"

let setup_progress = Term.(const setup_progress $ width)

let without_progress =
  let doc = "Don't print progress bar." in
  Arg.(value & flag & info [ "without-progress" ] ~doc)

let without_recognition =
  let doc = "Don't recognize MIME type of files." in
  Arg.(value & flag & info [ "without-recognition" ] ~doc)

let term =
  let open Term in
  const run
  $ setup_logs
  $ setup_progress
  $ without_progress
  $ without_recognition
  $ algorithm
  $ root
  $ pagesize
  $ output

let existing_filename =
  let doc = "The file whose MIME type needs to be determined." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.is_regular_file str -> Ok v
    | Ok v ->
        error_msgf "%a does not exists (or is not a regular file)" Fpath.pp v
    | Error _ as err -> err
  in
  let open Arg in
  required
  & pos 0 (some (conv (parser, Fpath.pp))) None
  & info [] ~doc ~docv:"FILENAME"

let mime =
  let info = Cmd.info "mime" in
  let term =
    let open Term in
    const run_mime $ setup_logs $ existing_filename
  in
  Cmd.v info term

let cmd =
  let info = Cmd.info "immuable" in
  let make = Cmd.(v (info "make")) term in
  Cmd.group ~default:term info [ make; mime ]

let () = Cmd.(exit @@ eval_result cmd)
