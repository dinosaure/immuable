let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let () = Logs_threaded.enable ()

let entry_of_filename ?tbl alg filename =
  assert (Fpath.is_dir_path filename = false);
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let stat = Unix.stat (Fpath.to_string filename) in
  match stat.Unix.st_kind with
  | Unix.S_REG ->
      let hdr = Fmt.str "blob %d\000" stat.Unix.st_size in
      let ctx = Hash.empty in
      let ctx = Hash.feed_string ctx hdr in
      let ic = open_in (Fpath.to_string filename) in
      let finally () = close_in ic in
      Fun.protect ~finally @@ fun () ->
      let buf = Bytes.create 0x7ff in
      let rec go ctx =
        let len = input ic buf 0 (Bytes.length buf) in
        if len > 0 then go (Hash.feed_bytes ctx ~off:0 ~len buf)
        else Hash.get ctx |> Hash.to_raw_string |> Carton.Uid.unsafe_of_string
      in
      let uid = go ctx in
      let len = stat.Unix.st_size in
      Option.iter (fun tbl -> Hashtbl.add tbl uid (`File (filename, len))) tbl;
      let length = stat.Unix.st_size in
      Cartonnage.Entry.make ~kind:`C ~length uid ()
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

let entry_of_commit ?tbl alg ~(root : Carton.Uid.t)
    ~(metadata : Carton.Uid.t) =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let str = Fmt.str "%s%s" (root :> string) (metadata :> string) in
  let hdr = Fmt.str "commit %d\000" (String.length str) in
  let ctx = Hash.empty in
  let ctx = Hash.feed_string ctx hdr in
  let ctx = Hash.feed_string ctx str in
  let uid = Hash.get ctx |> Hash.to_raw_string |> Carton.Uid.unsafe_of_string in
  Option.iter (fun tbl -> Hashtbl.add tbl uid (`Commit str)) tbl;
  Cartonnage.Entry.make ~kind:`A ~length:(String.length str) uid ()

let walk_on_files alg (rtbl, tbl) path entries =
  let entry = entry_of_filename ?tbl alg path in
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
    let* perm = match stat.Unix.st_kind with
      | Unix.S_REG when stat.Unix.st_perm = 0o755 -> Some `Exec
      | Unix.S_REG when stat.Unix.st_perm = 0o664 -> Some `Everybody
      | Unix.S_REG -> Some `Normal
      | Unix.S_DIR -> Some `Dir
      | Unix.S_LNK -> Some `Link
      | _ -> None in
    Logs.debug (fun m -> m "search %a" Fpath.pp abs);
    let* node = Hashtbl.find_opt rtbl abs in
    Some { Tree.perm; name= Fpath.basename rel; node } in
  let fn1 = walk_on_directories alg (rtbl, tbl) in
  let abss = List.map (fun rel -> Fpath.(path // rel)) lst in
  let entries = Bos.OS.Path.fold ~traverse:`None ~elements:`Dirs fn1 entries abss in
  let entries = Result.value ~default:[] entries in
  let tree = List.filter_map fn0 lst in
  let entry = entry_of_tree ?tbl alg tree in
  Logs.debug (fun m -> m "[+] %a" Fpath.pp path);
  Hashtbl.add rtbl path (Cartonnage.Entry.uid entry);
  entry :: entries

let null alg =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  String.make Hash.digest_size '\000' |> Carton.Uid.unsafe_of_string

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

let pack_of_filename alg filename =
  let tbl = Hashtbl.create 10 in
  let entry2 = entry_of_filename ~tbl alg filename in
  let node = Cartonnage.Entry.uid entry2 in
  let tree = [ { Tree.perm= `Normal; name= Fpath.filename filename; node } ] in
  let entry1 = entry_of_tree ~tbl alg tree in
  let metadata = null alg in
  let root = Cartonnage.Entry.uid entry1 in
  let entry0 = entry_of_commit ~tbl alg ~root ~metadata in
  let entries = [ entry0; entry1; entry2 ] in
  let targets = List.map Cartonnage.Target.make entries in
  let targets = List.to_seq targets in
  let with_header = 3 in
  let with_signature = signature alg in
  let load uid () =
    match Hashtbl.find tbl uid with
    | `Commit str -> Carton.Value.of_string ~kind:`A str
    | `Tree str -> Carton.Value.of_string ~kind:`B str
    | `File (filename, len) -> value_of_filename ~len filename
  in
  Carton_miou_unix.to_pack ~with_header ~with_signature ~load targets
  |> Result.ok

let pack_of_directory alg root =
  let tbl = Hashtbl.create 0x100
  and rtbl = Hashtbl.create 0x100 in
  let ( let* ) = Result.bind in
  let fn0 = walk_on_files alg (rtbl, Some tbl) in
  let* entries = Bos.OS.Path.fold ~elements:`Files fn0 [] [ root ] in
  let fn1 = walk_on_directories alg (rtbl, Some tbl) in
  let* entries = Bos.OS.Path.fold ~elements:`Dirs ~traverse:`None fn1 entries [ root ] in
  let root = Hashtbl.find_opt rtbl root in
  let* root = Option.to_result ~none:`Root_not_found root in
  let metadata = null alg in
  let entry0 = entry_of_commit ~tbl alg ~root ~metadata in
  let entries = entry0 :: entries in
  let load uid () =
    match Hashtbl.find tbl uid with
    | `Commit str -> Carton.Value.of_string ~kind:`A str
    | `Tree str -> Carton.Value.of_string ~kind:`B str
    | `File (filename, len) -> value_of_filename ~len filename in
  let with_header = List.length entries in
  let entries = List.to_seq entries in
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let ref_length = Hash.digest_size in
  let targets = Carton_miou_unix.delta ~ref_length ~load entries in
  let with_signature = signature alg in
  Carton_miou_unix.to_pack ~with_header ~with_signature ~load targets
  |> Result.ok

let run _ alg root _pagesize output =
  let ( let* ) = Result.bind in
  let* seq =
    if Fpath.is_dir_path root
    then pack_of_directory alg root
    else pack_of_filename alg root in
  let oc = open_out (Fpath.to_string output) in
  let finally () = close_out oc in
  Fun.protect ~finally @@ fun () ->
  Seq.iter (output_string oc) seq;
  Ok ()

let pp_error ppf = function
  | `Root_not_found -> Fmt.string ppf "Root not found"
  | `Msg msg -> Fmt.string ppf msg

let run quiet alg root pagesize output =
  Miou_unix.run @@ fun () ->
  run quiet alg root pagesize output
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
  required & opt (some output) None & info [ "o"; "output" ] ~doc ~docv:"OUTPUT"

let term =
  let open Term in
  const run $ setup_logs $ algorithm $ root $ pagesize $ output

let cmd =
  let info = Cmd.info "immuable" in
  Cmd.v info term

let () = Cmd.(exit @@ eval_result cmd)
