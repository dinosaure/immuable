let src = Logs.Src.create "immuable.tree"

module Log = (val Logs.src_log src : Logs.LOG)

type perm = [ `Normal | `Everybody | `Exec | `Link | `Dir | `Commit ]

let string_of_perm = function
  | `Normal -> "100644"
  | `Everybody -> "100664"
  | `Exec -> "100755"
  | `Link -> "120000"
  | `Dir -> "400000"
  | `Commit -> "160000"

let perm_of_string = function
  | "44" | "100644" -> `Normal
  | "100664" -> `Everybody
  | "100755" -> `Exec
  | "120000" -> `Link
  | "400000" | "040000" -> `Dir
  | "160000" -> `Commit
  | v -> Fmt.invalid_arg "perm_of_string: %S" v

type entry = { perm: perm; name: string; node: Carton.Uid.t }

let safe_exn fn x = try fn x with _ -> raise Encore.Bij.Bijection

let perm =
  let fwd = safe_exn perm_of_string in
  let bwd = safe_exn string_of_perm in
  Encore.Bij.v ~fwd ~bwd

let hash =
  let fwd str = Carton.Uid.unsafe_of_string str in
  let bwd (v : Carton.Uid.t) = (v :> string) in
  Encore.Bij.v ~fwd ~bwd

let entry =
  let fwd ((perm, name), node) = { perm; name; node } in
  let bwd { perm; name; node } = ((perm, name), node) in
  Encore.Bij.v ~fwd ~bwd

let is_not_sp = ( <> ) ' '
let is_not_nl = ( <> ) '\x00'

let entry ~ref_length =
  let open Encore.Syntax in
  let perm = perm <$> while1 is_not_sp in
  let hash = hash <$> fixed ref_length in
  let name = while1 is_not_nl in
  entry
  <$> (perm
      <* (Encore.Bij.char ' ' <$> any)
      <* commit
      <*> (name <* (Encore.Bij.char '\x00' <$> any) <* commit)
      <*> (hash <* commit)
      <* commit)

let fmt ~ref_length =
  let entry = entry ~ref_length in
  Encore.Syntax.rep0 entry 

let of_string ~ref_length str =
  let e = fmt ~ref_length in
  let a = Encore.to_angstrom e in
  let consume = Angstrom.Consume.All in
  match Angstrom.parse_string ~consume a str with
  | Ok tree -> Ok tree
  | Error err ->
      Log.err (fun m -> m "Invalid tree (ref_length: %d): %s" ref_length err);
      Log.err (fun m -> m "@[<hov>%a@]"
        (Hxd_string.pp Hxd.default) str);
      Error `Invalid_tree

let to_string ~ref_length tree =
  let e = fmt ~ref_length in
  let l = Encore.to_lavoisier e in
  Encore.Lavoisier.emit_string tree l
