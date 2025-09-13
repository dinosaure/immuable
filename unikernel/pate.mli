type entry = { offset: int; crc: Optint.t; consumed: int; size: int }

type config = {
    cachesize: int option
  ; ref_length: int
  ; identify: Carton.identify
  ; on_entry: max:int -> entry -> unit
  ; on_object: cursor:int -> Carton.Value.t -> Carton.Uid.t -> unit
}

val config :
     ?cachesize:int
  -> ?on_entry:(max:int -> entry -> unit)
  -> ?on_object:(cursor:int -> Carton.Value.t -> Carton.Uid.t -> unit)
  -> ref_length:int
  -> Carton.identify
  -> config

type delta = { source: Carton.Uid.t; depth: int; raw: Cachet.Bstr.t }

val entries_of_pack :
     cfg:config
  -> digest:Carton.First_pass.digest
  -> Mkernel.Block.t
  -> (Mkernel.Block.t Carton.t * delta option) Cartonnage.Entry.t array

val of_block :
     cfg:config
  -> digest:Carton.First_pass.digest
  -> name:string
  -> (Mkernel.Block.t Carton.t * delta option) Cartonnage.Entry.t array
     Mkernel.arg
