let src = Logs.Src.create "immuable"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind
let guard predicate fn = if predicate then Ok () else Error (fn ())
let invalid_immuable_image () = `Invalid_immuable_image
let invalid_immuable_commit () = `Invalid_immuable_commit
let unexpected_first_immuable_entry () = `Unexpected_first_immuable_entry
let open_error_msg = function Ok _ as v -> v | Error (`Msg _) as err -> err

module Tree = Immuable_tree

type error =
  [ `Invalid_immuable_image
  | `Invalid_immuable_commit
  | `Invalid_tree
  | `Unexpected_first_immuable_entry
  | `Not_found
  | `Msg of string ]

let pp_error ppf = function
  | `Invalid_immuable_image -> Fmt.string ppf "Invalid immuable image"
  | `Invalid_immuable_commit -> Fmt.string ppf "Invalid invalid_immuable_commit"
  | `Invalid_tree -> Fmt.string ppf "Invalid tree"
  | `Unexpected_first_immuable_entry ->
      Fmt.string ppf "Unexpected first immuable entry"
  | `Not_found -> Fmt.string ppf "Not found"
  | `Msg msg -> Fmt.string ppf msg

module Entry = struct
  type t = { str: string; etag: string; mime: string option }

  let weight { str; _ } = String.length str
end

module Cache = Lru.M.Make (String) (Entry)

let load pack uid =
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

let rec walk ~cfg pack dirs entries (current, node) =
  let str = Carton.Value.string (load pack node) in
  let ref_length = cfg.Pate.ref_length in
  let* tree = Tree.of_string ~ref_length str in
  let go acc elt =
    match (acc, elt) with
    | (Error _ as err), _ -> err
    | Ok (dirs, entries), { Tree.perm= `Dir; name; node } ->
        let current = Fpath.add_seg current name in
        let current = Fpath.to_dir_path current in
        let dirs = Fpath.Set.add current dirs in
        walk ~cfg pack dirs entries (current, node)
    | Ok (dirs, entries), { name; node; _ } ->
        let current = Fpath.add_seg current name in
        Log.debug (fun m -> m "[+] %a" Fpath.pp current);
        Ok (dirs, (current, node) :: entries)
  in
  List.fold_left go (Ok (dirs, entries)) tree

let walk ~cfg pack root = walk ~cfg pack Fpath.Set.empty [] (Fpath.v "/", root)

type t = {
    tree: Carton.Uid.t Art.t
  ; dirs: Fpath.Set.t
  ; pack: Mkernel.Block.t Carton.t
  ; mime: string Art.t
  ; cache: Cache.t
}

let fill_mime_database pack hash =
  let str = Carton.Value.string (load pack hash) in
  let entries = String.split_on_char '\000' str in
  let mime = Art.make () in
  let rec go = function
    | [] | [ "" ] -> ()
    | filepath :: value :: rest ->
        Log.debug (fun m -> m "MIME of %s: %s" filepath value);
        let key = Art.key ("/" ^ filepath) in
        Art.insert mime key value; go rest
    | _ :: _ -> Log.warn (fun m -> m "Malformed MIME database")
  in
  go entries; mime

let fs ~cfg ~cache entries =
  let* () = guard (Array.length entries >= 3) invalid_immuable_image in
  let predicate = Cartonnage.Entry.kind entries.(0) = `A in
  let* () = guard predicate unexpected_first_immuable_entry in
  let commit = entries.(0) in
  let pack, _ = Cartonnage.Entry.meta commit in
  let* root, metadata = get_root_and_metadata ~cfg pack commit in
  let mime = fill_mime_database pack metadata in
  let* dirs, files = walk ~cfg pack root in
  let tree = Art.make () in
  let fn (path, uid) =
    let key = Art.key (Fpath.to_string path) in
    Art.insert tree key uid
  in
  List.iter fn files;
  let cache = Cache.create cache in
  Ok { tree; dirs; pack; mime; cache }

let copy { tree; dirs; pack; mime; cache } =
  { tree; dirs; pack= Carton.copy pack; mime; cache }

let of_block ~cfg ~digest ~name ~cache =
  let v blk () =
    let entries = Pate.entries_of_pack ~cfg ~digest blk in
    match fs ~cfg ~cache entries with
    | Ok t -> t
    | Error err ->
        Fmt.failwith "Impossible to load given immuable image: %a" pp_error err
  in
  Mkernel.map v [ Mkernel.block name ]

let find t path =
  let* path = Fpath.of_string path |> open_error_msg in
  let path =
    if Fpath.is_dir_path path || Fpath.Set.mem (Fpath.to_dir_path path) t.dirs
    then Fpath.(path / "index.html")
    else path
  in
  let filepath = Fpath.to_string path in
  match Cache.find filepath t.cache with
  | Some entry ->
      Cache.promote filepath t.cache;
      Ok (entry.str, entry.mime)
  | None ->
      begin try
        let key = Art.key filepath in
        let uid = Art.find t.tree key in
        let value = load t.pack uid in
        let bstr = Carton.Value.bigstring value in
        let len = Carton.Value.length value in
        let str = Bstr.sub_string bstr ~off:0 ~len in
        let mime = Art.find_opt t.mime key in
        let etag = Ohex.encode (uid :> string) in
        Cache.add filepath { str; etag; mime } t.cache;
        Cache.trim t.cache;
        Ok (str, mime)
      with exn ->
        Log.err (fun m ->
            m "Got an exception when we tried to find %s: %s" filepath
              (Printexc.to_string exn));
        Error `Not_found
      end

let etag t path =
  let* path = Fpath.of_string path |> open_error_msg in
  let path =
    if Fpath.is_dir_path path || Fpath.Set.mem (Fpath.to_dir_path path) t.dirs
    then Fpath.(path / "index.html")
    else path
  in
  let filepath = Fpath.to_string path in
  match Cache.find filepath t.cache with
  | Some entry ->
      Cache.promote filepath t.cache;
      Ok entry.etag
  | None ->
      begin try
        let hash = Art.find t.tree (Art.key filepath) in
        Ok (Ohex.encode (hash :> string))
      with _ -> Error `Not_found
      end

let if_match t req target =
  match etag t target with
  | Error _ -> false
  | Ok hash -> begin
      let hdrs = Vifu.Request.headers req in
      match Vifu.Headers.get hdrs "if-none-match" with
      | Some hash' -> String.equal hash hash'
      | None -> false
    end

let handler ~pool =
  ();
  fun req target server _ ->
    let open Vifu.Response.Syntax in
    let pool = Vifu.Server.device pool server in
    Cattery.use pool @@ fun t ->
    let target = String.split_on_char '?' target |> List.hd in
    match find t target with
    | Ok _ when if_match t req target ->
        let process =
          let* () = Vifu.Response.with_string req "" in
          Vifu.Response.respond `Not_modified
        in
        Some process
    | Ok (str, mime) ->
        let process =
          let field = "content-length" in
          let value = string_of_int (String.length str) in
          let* () = Vifu.Response.add ~field value in
          let field = "etag" in
          let etag = Result.get_ok (etag t target) in
          let* () = Vifu.Response.add ~field (etag :> string) in
          let* () =
            match mime with
            | Some mime -> Vifu.Response.add ~field:"Content-Type" mime
            | None -> Vifu.Response.return ()
          in
          let* () = Vifu.Response.with_string req str in
          Vifu.Response.respond `OK
        in
        Some process
    | Error _ ->
        Log.err (fun m -> m "Target %s not found" target);
        None
