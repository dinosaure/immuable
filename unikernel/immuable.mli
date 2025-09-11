type t

type error =
  [ `Invalid_immuable_image
  | `Invalid_immuable_commit
  | `Invalid_tree
  | `Unexpected_first_immuable_entry
  | `Not_found ]

val fs :
     cfg:Carton_mkernel.config
  -> (Mkernel.Block.t Carton.t * 'a) Cartonnage.Entry.t array
  -> (t, [> error ]) result

val find : t -> string -> (Bstr.t, [> error ]) result
val etag : t -> string -> (string, [> error ]) result

val of_block :
     cfg:Carton_mkernel.config
  -> digest:Carton.First_pass.digest
  -> name:string
  -> t Mkernel.arg

val handler : t -> ('c, 'value) Vifu.Handler.t
