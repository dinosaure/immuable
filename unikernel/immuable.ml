let src = Logs.Src.create "immuable"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind
let guard predicate fn = if predicate then Ok () else Error (fn ())
let invalid_immuable_image () = `Invalid_immuable_image
let invalid_immuable_commit () = `Invalid_immuable_commit
let unexpected_first_immuable_entry () = `Unexpected_first_immuable_entry

module Tree = Immuable_tree

type error =
  [ `Invalid_immuable_image
  | `Invalid_immuable_commit
  | `Invalid_tree
  | `Unexpected_first_immuable_entry
  | `Not_found ]

let pp_error ppf = function
  | `Invalid_immuable_image -> Fmt.string ppf "Invalid immuable image"
  | `Invalid_immuable_commit -> Fmt.string ppf "Invalid invalid_immuable_commit"
  | `Invalid_tree -> Fmt.string ppf "Invalid tree"
  | `Unexpected_first_immuable_entry ->
      Fmt.string ppf "Unexpected first immuable entry"
  | `Not_found -> Fmt.string ppf "Not found"

let load pack uid =
  Log.debug (fun m -> m "load %a" Carton.Uid.pp uid);
  let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
  let blob = Carton.Blob.make ~size in
  Carton.of_uid pack blob ~uid

let get_root_and_metadata ~cfg pack commit =
  let str = Carton.Value.string (load pack (Cartonnage.Entry.uid commit)) in
  let ref_length = cfg.Pate.ref_length in
  let predicate = String.length str >= 2 * ref_length in
  let* () = guard predicate invalid_immuable_commit in
  let root = String.sub str 0 ref_length in
  let root = Carton.Uid.unsafe_of_string root in
  let metadata = String.sub str ref_length ref_length in
  let metadata = Carton.Uid.unsafe_of_string metadata in
  Ok (root, metadata)

let rec walk ~cfg pack entries (current, node) =
  let str = Carton.Value.string (load pack node) in
  let ref_length = cfg.Pate.ref_length in
  let* tree = Tree.of_string ~ref_length str in
  let go acc elt =
    match (acc, elt) with
    | (Error _ as err), _ -> err
    | Ok entries, { Tree.perm= `Dir; name; node } ->
        let current = Fpath.add_seg current name in
        let current = Fpath.to_dir_path current in
        walk ~cfg pack entries (current, node)
    | Ok entries, { name; node; _ } ->
        let current = Fpath.add_seg current name in
        Log.debug (fun m -> m "[+] %a" Fpath.pp current);
        Ok ((current, node) :: entries)
  in
  List.fold_left go (Ok entries) tree

let walk ~cfg pack root = walk ~cfg pack [] (Fpath.v "/", root)

type t = {
    tree: Carton.Uid.t Art.t
  ; pack: Mkernel.Block.t Carton.t
  ; mime: string Art.t
}

let fill_mime_database pack hash =
  let str = Carton.Value.string (load pack hash) in
  let entries = String.split_on_char '\000' str in
  let mime = Art.make () in
  let rec go = function
    | [] | [""]-> ()
    | filepath :: value :: rest ->
        Log.debug (fun m -> m "MIME of %s: %s" filepath value);
        let key = Art.key ("/" ^ filepath) in
        Art.insert mime key value; go rest
    | _ :: _ -> Log.warn (fun m -> m "Malformed MIME database")
  in
  go entries; mime

let fs ~cfg entries =
  let* () = guard (Array.length entries >= 3) invalid_immuable_image in
  let predicate = Cartonnage.Entry.kind entries.(0) = `A in
  let* () = guard predicate unexpected_first_immuable_entry in
  let commit = entries.(0) in
  let pack, _ = Cartonnage.Entry.meta commit in
  let* root, metadata = get_root_and_metadata ~cfg pack commit in
  let mime = fill_mime_database pack metadata in
  let* files = walk ~cfg pack root in
  let tree = Art.make () in
  let fn (path, uid) =
    let key = Art.key (Fpath.to_string path) in
    Art.insert tree key uid
  in
  List.iter fn files;
  Ok { tree; pack; mime }

let copy { tree; pack; mime } = { tree; pack= Carton.copy pack; mime }

let of_block ~cfg ~digest ~name =
  let v blk () =
    let entries = Pate.entries_of_pack ~cfg ~digest blk in
    match fs ~cfg entries with
    | Ok t -> t
    | Error err ->
        Fmt.failwith "Impossible to load given immuable image: %a" pp_error err
  in
  Mkernel.map v [ Mkernel.block name ]

let find t filepath =
  try
    let key = Art.key filepath in
    let uid = Art.find t.tree key in
    Log.debug (fun m -> m "%s -> %a" filepath Carton.Uid.pp uid);
    let value = load t.pack uid in
    let bstr = Carton.Value.bigstring value in
    let mime = Art.find_opt t.mime key in
    Ok (Bstr.sub bstr ~off:0 ~len:(Carton.Value.length value), mime)
  with exn ->
    Log.err (fun m ->
        m "Got an exception when we tried to find %s: %s" filepath
          (Printexc.to_string exn));
    Error `Not_found

let etag t filepath =
  try
    let hash = Art.find t.tree (Art.key filepath) in
    Ok (Ohex.encode (hash :> string))
  with _ -> Error `Not_found

let if_match t req target =
  let hdrs = Vifu.Request.headers req in
  let hash = Result.get_ok (etag t target) in
  match Vifu.Headers.get hdrs "if-none-match" with
  | Some hash' -> String.equal (hash :> string) (hash' :> string)
  | None -> false

let handler ~pool =
  ();
  fun req target server _ ->
    let open Vifu.Response.Syntax in
    let pool = Vifu.Server.device pool server in
    Cattery.use pool @@ fun t ->
    match find t target with
    | Ok _ when if_match t req target ->
        let process =
          let* () = Vifu.Response.with_string req "" in
          Vifu.Response.respond `Not_modified
        in
        Some process
    | Ok (bstr, mime) ->
        let process =
          let field = "content-length" in
          let value = string_of_int (Bstr.length bstr) in
          let* () = Vifu.Response.add ~field value in
          let field = "etag" in
          let etag = Result.get_ok (etag t target) in
          let* () = Vifu.Response.add ~field (etag :> string) in
          let* () =
            match mime with
            | Some mime -> Vifu.Response.add ~field:"Content-Type" mime
            | None -> Vifu.Response.return ()
          in
          let str = Bstr.to_string bstr in
          let* () = Vifu.Response.with_string req str in
          Vifu.Response.respond `OK
        in
        Some process
    | Error _ ->
        Log.err (fun m -> m "Target %s not found" target);
        None
