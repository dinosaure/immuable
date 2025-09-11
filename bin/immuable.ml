let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let () = Logs_threaded.enable ()

let uid_and_length_of_filename ?tbl alg filename =
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
      (uid, stat.Unix.st_size)
  | Unix.S_DIR -> assert false
  | Unix.S_CHR | S_BLK | S_FIFO | S_SOCK ->
      Fmt.failwith "Impossible to save %a" Fpath.pp filename
  | Unix.S_LNK -> assert false (* TODO *)

module Tree = Immuable_tree

let uid_and_length_of_tree ?tbl alg tree =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let ref_length = Hash.digest_size in
  let str = Tree.to_string ~ref_length tree in
  let hdr = Fmt.str "tree %d\000" (String.length str) in
  let ctx = Hash.empty in
  let ctx = Hash.feed_string ctx hdr in
  let ctx = Hash.feed_string ctx str in
  let uid = Hash.get ctx |> Hash.to_raw_string |> Carton.Uid.unsafe_of_string in
  Option.iter (fun tbl -> Hashtbl.add tbl uid (`Tree str)) tbl;
  (uid, String.length str)

let uid_and_length_of_commit ?tbl alg ~(root : Carton.Uid.t)
    ~(metadata : Carton.Uid.t) =
  let module Hash = (val Digestif.module_of_hash' (alg :> Digestif.hash')) in
  let str = Fmt.str "%s%s" (root :> string) (metadata :> string) in
  let hdr = Fmt.str "commit %d\000" (String.length str) in
  let ctx = Hash.empty in
  let ctx = Hash.feed_string ctx hdr in
  let ctx = Hash.feed_string ctx str in
  let uid = Hash.get ctx |> Hash.to_raw_string |> Carton.Uid.unsafe_of_string in
  Option.iter (fun tbl -> Hashtbl.add tbl uid (`Commit str)) tbl;
  (uid, String.length str)

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
  let node, length = uid_and_length_of_filename ~tbl alg filename in
  let entrie2 = Cartonnage.Entry.make ~kind:`C ~length node () in
  let tree = [ { Tree.perm= `Normal; name= Fpath.filename filename; node } ] in
  let root, length = uid_and_length_of_tree ~tbl alg tree in
  let entrie1 = Cartonnage.Entry.make ~kind:`B ~length root () in
  let metadata = null alg in
  let commit, length = uid_and_length_of_commit ~tbl alg ~root ~metadata in
  let entrie0 = Cartonnage.Entry.make ~kind:`A ~length commit () in
  let entries = [ entrie0; entrie1; entrie2 ] in
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
  let total =
    let fn _ elt acc =
      match elt with
      | `Commit str | `Tree str -> acc + String.length str + acc
      | `File (_, len) -> acc + len
    in
    Hashtbl.fold fn tbl 0
  in
  let seq =
    Carton_miou_unix.to_pack ~with_header ~with_signature ~load targets
  in
  (total, seq)

let run _ alg root _pagesize output =
  if Fpath.is_dir_path root then assert false;
  let _total, seq = pack_of_filename alg root in
  let oc = open_out (Fpath.to_string output) in
  let finally () = close_out oc in
  Fun.protect ~finally @@ fun () ->
  let size = ref 0 in
  let write str =
    output_string oc str;
    size := String.length str + !size
  in
  Seq.iter write seq

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

let () = Cmd.(exit @@ eval cmd)
