type t

type error =
  [ `Invalid_immuable_image
  | `Invalid_immuable_commit
  | `Invalid_tree
  | `Unexpected_first_immuable_entry
  | `Not_found ]

val fs :
     cfg:Pate.config
  -> (Mkernel.Block.t Carton.t * 'a) Cartonnage.Entry.t array
  -> (t, [> error ]) result

val copy : t -> t
val find : t -> string -> (Bstr.t * string option, [> error ]) result
val etag : t -> string -> (string, [> error ]) result
val if_match : t -> ('c, 'v) Vifu.Request.t -> string -> bool

val of_block :
     cfg:Pate.config
  -> digest:Carton.First_pass.digest
  -> name:string
  -> t Mkernel.arg

val handler :
  pool:('a, t Cattery.t) Vifu.Device.device -> ('c, 'value) Vifu.Handler.t
