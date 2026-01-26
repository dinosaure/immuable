let src = Logs.Src.create "carton-mkernel"

module Log = (val Logs.src_log src : Logs.LOG)

let ignore3 ~cursor:_ _ _ = ()
let ignorem ~max:_ _ = ()

type entry = { offset: int; crc: Optint.t; consumed: int; size: int }

type config = {
    cachesize: int option
  ; ref_length: int
  ; identify: Carton.identify
  ; on_entry: max:int -> entry -> unit
  ; on_object: cursor:int -> Carton.Value.t -> Carton.Uid.t -> unit
}

let config ?cachesize ?(on_entry = ignorem) ?(on_object = ignore3) ~ref_length
    identify =
  { cachesize; ref_length; identify; on_entry; on_object }

type base = { value: Carton.Value.t; uid: Carton.Uid.t; depth: int }

(** the core of the verification *)

let identify (Carton.Identify gen) ~kind ~len bstr =
  let ctx = gen.Carton.First_pass.init kind (Carton.Size.of_int_exn len) in
  let ctx = gen.Carton.First_pass.feed (Bigarray.Array1.sub bstr 0 len) ctx in
  gen.Carton.First_pass.serialize ctx

let rec resolve_tree ?(on = ignore3) t oracle matrix ~base = function
  | [||] -> ()
  | [| cursor |] ->
      Log.debug (fun m -> m "resolve node at %08x" cursor);
      Log.debug (fun m ->
          m "blob: %d byte(s)"
            (Carton.(Blob.size (Value.blob base.value)) :> int));
      let value = Carton.of_offset_with_source t base.value ~cursor in
      let len = Carton.Value.length value
      and bstr = Carton.Value.bigstring value
      and kind = Carton.Value.kind value in
      let uid = identify oracle.Carton.identify ~kind ~len bstr
      and pos = oracle.where ~cursor
      and crc = oracle.checksum ~cursor
      and depth = succ base.depth in
      on ~cursor value uid;
      matrix.(pos) <-
        Carton.Resolved_node { cursor; uid; crc; kind; depth; parent= base.uid };
      let children = oracle.children ~cursor ~uid in
      let children = Array.of_list children in
      let value = Carton.Value.flip value in
      let base = { value; uid; depth } in
      resolve_tree ~on t oracle matrix ~base children
  | cursors ->
      let source = Carton.Value.source base.value in
      let source = Bstr.copy source in
      let rec go idx =
        if idx < Array.length cursors then begin
          let cursor = cursors.(idx) in
          Log.debug (fun m -> m "resolve node at %08x" cursor);
          Log.debug (fun m ->
              m "blob: %d byte(s)"
                (Carton.(Blob.size (Value.blob base.value)) :> int));
          let dirty = Carton.Value.source base.value in
          let src = Carton.Value.with_source ~source base.value in
          let value = Carton.of_offset_with_source t src ~cursor in
          let len = Carton.Value.length value
          and bstr = Carton.Value.bigstring value
          and kind = Carton.Value.kind value in
          let uid = identify oracle.Carton.identify ~kind ~len bstr
          and pos = oracle.where ~cursor
          and crc = oracle.checksum ~cursor
          and depth = succ base.depth in
          on ~cursor value uid;
          Log.debug (fun m -> m "resolve node %d/%d" pos (Array.length matrix));
          matrix.(pos) <-
            Resolved_node { cursor; uid; crc; kind; depth; parent= base.uid };
          let children = oracle.children ~cursor ~uid in
          Log.debug (fun m ->
              m "resolve children of %08x %a" cursor Carton.Uid.pp uid);
          let children = Array.of_list children in
          let value = Carton.Value.with_source ~source:dirty value in
          let value = Carton.Value.flip value in
          let base = { value; uid; depth } in
          resolve_tree ~on t oracle matrix ~base children;
          go (succ idx)
        end
      in
      go 0

let is_unresolved_base = function
  | Carton.Unresolved_base _ -> true
  | _ -> false

let verify ?(on = ignore3) t oracle matrix =
  let mutex = Miou.Mutex.create () in
  let idx = Atomic.make 0 in
  let rec fn t =
    let pos =
      Miou.Mutex.protect mutex @@ fun () ->
      while
        Atomic.get idx < Array.length matrix
        && is_unresolved_base matrix.(Atomic.get idx) = false
      do
        Atomic.incr idx
      done;
      Atomic.fetch_and_add idx 1
    in
    if pos < Array.length matrix then begin
      let[@warning "-8"] (Carton.Unresolved_base { cursor }) = matrix.(pos) in
      let size = oracle.Carton.size ~cursor in
      Log.debug (fun m -> m "resolve base (object %d) at %08x" pos cursor);
      Log.debug (fun m -> m "allocate a blob of %d byte(s)" (size :> int));
      let blob = Carton.Blob.make ~size in
      let value = Carton.of_offset t blob ~cursor in
      let len = Carton.Value.length value
      and bstr = Carton.Value.bigstring value
      and kind = Carton.Value.kind value in
      let uid = identify oracle.Carton.identify ~kind ~len bstr
      and crc = oracle.checksum ~cursor in
      on ~cursor value uid;
      matrix.(pos) <- Resolved_base { cursor; uid; crc; kind };
      let children = oracle.children ~cursor ~uid in
      let children = Array.of_list children in
      let base = Carton.{ value= Value.flip value; uid; depth= 1 } in
      resolve_tree ~on t oracle matrix ~base children;
      fn t
    end
  in
  fn t

let compile ?(on = ignorem) ~identify ~digest_length seq =
  let children_by_offset = Hashtbl.create 0x7ff in
  let children_by_uid = Hashtbl.create 0x7ff in
  let sizes : (int, Carton.Size.t ref) Hashtbl.t = Hashtbl.create 0x7ff in
  let where = Hashtbl.create 0x7ff in
  let crcs = Hashtbl.create 0x7ff in
  let is_base = Hashtbl.create 0x7ff in
  let index = Hashtbl.create 0x7ff in
  let ref_index = Hashtbl.create 0x7ff in
  let cursors = Hashtbl.create 0x7ff in
  let hash = ref (String.make digest_length '\000') in
  let update_size ~parent offset (size : Carton.Size.t) =
    Log.debug (fun m ->
        m "Update the size of %08x (parent: %08x) to %d byte(s)" offset parent
          (size :> int));
    let cell : Carton.Size.t ref = Hashtbl.find sizes parent in
    (cell := Carton.Size.(max !cell size));
    Hashtbl.replace sizes offset cell
  in
  let new_child ~parent child =
    match parent with
    | `Ofs parent -> begin
        match Hashtbl.find_opt children_by_offset parent with
        | None -> Hashtbl.add children_by_offset parent [ child ]
        | Some offsets ->
            Hashtbl.replace children_by_offset parent (child :: offsets)
      end
    | `Ref parent -> begin
        match Hashtbl.find_opt children_by_uid parent with
        | None -> Hashtbl.add children_by_uid parent [ child ]
        | Some offsets ->
            Hashtbl.replace children_by_uid parent (child :: offsets)
      end
  in
  let number_of_objects = ref 0 in
  let (Carton.Identify i) = identify in
  let ctx = ref None in
  let fn = function
    | `Number n -> number_of_objects := n
    | `Hash value -> hash := value
    | `Inflate (None, _) -> ()
    | `Inflate (Some (k, size), str) -> begin
        let open Carton in
        let open First_pass in
        match !ctx with
        | None ->
            let ctx0 = i.init k (Carton.Size.of_int_exn size) in
            let ctx0 = i.feed (Bstr.of_string str) ctx0 in
            ctx := Some ctx0
        | Some ctx0 ->
            let ctx0 = i.feed (Bstr.of_string str) ctx0 in
            ctx := Some ctx0
      end
    | `Entry entry -> begin
        let offset = entry.Carton.First_pass.offset in
        let size = entry.Carton.First_pass.size in
        let crc = entry.Carton.First_pass.crc in
        let consumed = entry.Carton.First_pass.consumed in
        on ~max:!number_of_objects { offset; crc; consumed; size:> int };
        Hashtbl.add where offset entry.number;
        Hashtbl.add cursors entry.number offset;
        Hashtbl.add crcs offset crc;
        match entry.Carton.First_pass.kind with
        | Carton.First_pass.Base kind ->
            Hashtbl.add sizes offset (ref size);
            Hashtbl.add is_base entry.number offset;
            let uid =
              match Option.map i.serialize !ctx with
              | Some uid -> uid
              | None ->
                  let size = entry.Carton.First_pass.size in
                  let ctx = i.init kind size in
                  i.serialize ctx
            in
            ctx := None;
            Hashtbl.add index uid offset
        | Ofs { sub; source; target; _ } ->
            Log.debug (fun m ->
                m "new OBJ_OFS object at %08x (rel: %08x)" offset sub);
            let abs_parent = offset - sub in
            update_size ~parent:abs_parent offset
              (Carton.Size.max target source);
            Log.debug (fun m ->
                m "new child for %08x at %08x" abs_parent offset);
            new_child ~parent:(`Ofs abs_parent) offset
        | Ref { ptr; source; target; _ } ->
            Log.debug (fun m ->
                m
                  "new OBJ_REF object at %08x (ptr: %a) (source: %d, target: \
                   %d)"
                  offset Carton.Uid.pp ptr
                  (source :> int)
                  (target :> int));
            begin match Hashtbl.find_opt index ptr with
            | Some parent ->
                update_size ~parent offset (Carton.Size.max source target)
            | None ->
                Hashtbl.add sizes offset (ref (Carton.Size.max source target))
            end;
            Hashtbl.add ref_index offset ptr;
            new_child ~parent:(`Ref ptr) offset
      end
  in
  Seq.iter fn seq;
  Hashtbl.iter
    (fun offset ptr ->
      match Hashtbl.find_opt index ptr with
      | Some parent ->
          Log.debug (fun m ->
              m "Update the size of %08x (parent: %08x, %a)" offset parent
                Carton.Uid.pp ptr);
          update_size ~parent offset !(Hashtbl.find sizes offset)
      | None -> ())
    ref_index;
  Log.debug (fun m -> m "%d object(s)" !number_of_objects);
  let children ~cursor ~uid =
    match
      ( Hashtbl.find_opt children_by_offset cursor
      , Hashtbl.find_opt children_by_uid uid )
    with
    | Some (_ :: _ as children), (Some [] | None) -> children
    | (Some [] | None), Some (_ :: _ as children) -> children
    | (None | Some []), (None | Some []) -> []
    | Some lst0, Some lst1 ->
        List.(sort_uniq Int.compare (rev_append lst0 lst1))
  in
  let where ~cursor = Hashtbl.find where cursor in
  let size ~cursor = !(Hashtbl.find sizes cursor) in
  let checksum ~cursor = Hashtbl.find crcs cursor in
  let is_base ~pos = Hashtbl.find_opt is_base pos in
  let cursor ~pos = Hashtbl.find cursors pos in
  {
    Carton.identify
  ; children
  ; where
  ; cursor
  ; size
  ; checksum
  ; is_base
  ; number_of_objects= !number_of_objects
  ; hash= !hash
  }

(** generate entries to pack from a pack file *)

type delta = { source: Carton.Uid.t; depth: int; raw: Cachet.Bstr.t }

let seq_of_blk blk =
  let pagesize = Mkernel.Block.pagesize blk in
  let buf = Bstr.create pagesize in
  let src_off = ref 0 in
  let rec go () =
    Mkernel.Block.read blk ~src_off:!src_off buf;
    src_off := !src_off + pagesize;
    Seq.Cons (Bstr.to_string buf, go)
  in
  go

let entries_of_pack ~cfg ~digest blk =
  let raw = Hashtbl.create 0x7ff in
  let z = Bstr.create De.io_buffer_size in
  let t =
    let map blk ~pos len =
      let bstr = Bstr.create len in
      Mkernel.Block.read blk ~src_off:pos bstr;
      bstr
    in
    let pagesize = Mkernel.Block.pagesize blk in
    let z = Bstr.create De.io_buffer_size in
    let allocate _ = De.make_window ~bits:15 in
    let index uid =
      Fmt.failwith "Impossible to find the object %a" Carton.Uid.pp uid
    in
    Carton.make ~pagesize ?cachesize:cfg.cachesize ~map blk ~z ~allocate
      ~ref_length:cfg.ref_length index
  in
  let on ~max:_ entry =
    let cursor = entry.offset and consumed = entry.consumed in
    let bstr = Carton.map t ~cursor ~consumed in
    Hashtbl.add raw cursor bstr
  in
  let seq = seq_of_blk blk in
  let seq =
    let window = De.make_window ~bits:15 in
    let allocate _bits = window in
    Carton.First_pass.of_seq ~output:z ~allocate ~ref_length:cfg.ref_length
      ~digest seq
  in
  let (Carton.First_pass.Digest ({ length= digest_length; _ }, _)) = digest in
  let oracle = compile ~on ~identify:cfg.identify ~digest_length seq in
  let matrix =
    Array.init oracle.Carton.number_of_objects @@ fun pos ->
    match oracle.is_base ~pos with
    | Some cursor -> Carton.Unresolved_base { cursor }
    | None -> Unresolved_node { cursor= oracle.Carton.cursor ~pos }
  in
  let size = Hashtbl.create 0x7ff in
  let on ~cursor:_ value uid =
    Hashtbl.add size uid (Carton.Value.length value)
  in
  verify ~on t oracle matrix;
  let idx = Hashtbl.create 0x7ff in
  let t = Carton.with_index t (Hashtbl.find idx) in
  let fn = function
    | Carton.Unresolved_base _ -> assert false
    | Unresolved_node _ -> assert false
    | Resolved_base { cursor; uid; kind; _ } ->
        Hashtbl.add idx uid (Carton.Local cursor);
        let length = Hashtbl.find size uid in
        let meta = (t, None) in
        Cartonnage.Entry.make ~kind uid ~length:(length :> int) meta
    | Resolved_node { cursor; uid; kind; depth; parent; _ } ->
        Hashtbl.add idx uid (Carton.Local cursor);
        let raw = Hashtbl.find raw cursor in
        let delta = { source= parent; depth; raw } in
        let meta = (t, Some delta) in
        let length = Hashtbl.find size uid in
        Cartonnage.Entry.make ~kind uid ~length:(length :> int) meta
  in
  Array.map fn matrix

let of_block ~cfg ~digest ~name =
  let entries_of_block blk () = entries_of_pack ~cfg ~digest blk in
  Mkernel.map entries_of_block [ Mkernel.block name ]
