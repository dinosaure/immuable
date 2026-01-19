module RNG = Mirage_crypto_rng.Fortuna
module Immuable = Immuable

let index fs req _server _user's_value =
  let open Vifu.Response.Syntax in
  match Immuable.find fs "/index.html" with
  | Ok _ when Immuable.if_match fs req "/index.html" ->
      let* () = Vifu.Response.with_string req "" in
      Vifu.Response.respond `Not_modified
  | Ok bstr ->
      let str = Bstr.to_string bstr in
      let etag = Result.get_ok (Immuable.etag fs "/index.html") in
      let field = "ETag" in
      let* () = Vifu.Response.add ~field (etag :> string) in
      let* () = Vifu.Response.with_string req str in
      Vifu.Response.respond `OK
  | Error _ ->
      let str = Fmt.str "/index.html not found" in
      let* () = Vifu.Response.with_string req str in
      Vifu.Response.respond `Not_found

let _1s = 1_000_000_000
let rec gc () = Gc.compact (); Mkernel.sleep _1s; gc ()

let run _ (cfg, digest) cidr gateway port =
  let devices =
    let open Mkernel in
    [
      Mnet.stackv4 ~name:"service" ?gateway cidr
    ; Immuable.of_block ~cfg ~digest ~name:"immuable"
    ]
  in
  Mkernel.run devices @@ fun (daemon, tcpv4, _udpv4) fs () ->
  let rng = Mirage_crypto_rng_mkernel.initialize (module RNG) in
  let gc = Miou.async gc in
  let finally () =
    Mirage_crypto_rng_mkernel.kill rng;
    Mnet.kill daemon;
    Miou.cancel gc
  in
  Fun.protect ~finally @@ fun () ->
  let cfg = Vifu.Config.v port in
  let handlers = [ Immuable.handler fs ] in
  let routes =
    let open Vifu.Route in
    let open Vifu.Uri in
    [ get (rel /?? any) --> index fs ]
  in
  Vifu.run ~cfg ~handlers tcpv4 routes ()

open Cmdliner

let output_options = "OUTPUT OPTIONS"
let verbosity = Logs_cli.level ~docs:output_options ()
let renderer = Fmt_cli.style_renderer ~docs:output_options ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc)

let t0 = Mkernel.clock_monotonic ()
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let neg fn = fun x -> not (fn x)

let reporter sources ppf =
  let re = Option.map Re.compile sources in
  let print src =
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      let t1 = Mkernel.clock_monotonic () in
      let delta = Float.of_int (t1 - t0) in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let regexp =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value & opt_all regexp [ ("", `None) ] & info [ "l" ] ~doc ~docv:"REGEXP"

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let res =
        List.fold_left
          (fun acc -> function `Re re -> re :: acc | _ -> acc)
          [] res
      in
      Some (Re.alt res)

let setup_sources = Term.(const setup_sources $ sources)

let setup_logs utf_8 style_renderer sources level =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter sources Fmt.stdout);
  Option.is_none level

let setup_logs =
  Term.(const setup_logs $ utf_8 $ renderer $ setup_sources $ verbosity)

let ipv4 =
  let doc = "The IP address of the unikernel." in
  let ipaddr = Arg.conv (Ipaddr.V4.Prefix.of_string, Ipaddr.V4.Prefix.pp) in
  let open Arg in
  required & opt (some ipaddr) None & info [ "ipv4" ] ~doc ~docv:"IPv4"

let ipv4_gateway =
  let doc = "The IP gateway." in
  let ipaddr = Arg.conv (Ipaddr.V4.of_string, Ipaddr.V4.pp) in
  let open Arg in
  value & opt (some ipaddr) None & info [ "ipv4-gateway" ] ~doc ~docv:"IPv4"

let port =
  let doc = "The HTTP port" in
  let open Arg in
  value & opt int 80 & info [ "p"; "port" ] ~doc ~docv:"PORT"

let setup_signature hash =
  let module Hash = (val Digestif.module_of_hash' (hash :> Digestif.hash')) in
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

let signature =
  let doc = "The hash algorithm used to verify the immuable image integrity." in
  let open Arg in
  value & opt alg `SHA1 & info [ "signature" ] ~doc ~docv:"HASH"

let algorithm =
  let doc = "The hash algorithm used to identify immuable entries." in
  let open Arg in
  value & opt alg `SHA1 & info [ "a"; "algorithm" ] ~doc ~docv:"HASH"

let setup_signature = Term.(const setup_signature $ signature)

let cachesize =
  let doc = "The size of the cache (must be a power of two)." in
  let open Arg in
  value & opt int 0x100 & info [ "cachesize" ] ~doc ~docv:"SIZE"

let git_identify hash =
  let module Hash = (val Digestif.module_of_hash' (hash :> Digestif.hash')) in
  let pp_kind ppf = function
    | `A -> Fmt.string ppf "commit"
    | `B -> Fmt.string ppf "tree"
    | `C -> Fmt.string ppf "blob"
    | `D -> Fmt.string ppf "tag"
  in
  let init kind (len : Carton.Size.t) =
    let hdr = Fmt.str "%a %d\000" pp_kind kind (len :> int) in
    let ctx = Hash.empty in
    Hash.feed_string ctx hdr
  in
  let feed bstr ctx = Hash.feed_bigstring ctx bstr in
  let ( $ ) = Fun.compose in
  let serialize = Hash.(Carton.Uid.unsafe_of_string $ to_raw_string $ get) in
  Carton.Identify { Carton.First_pass.init; feed; serialize }

let setup_carton_config hash cachesize digest =
  let identify = git_identify hash in
  let ref_length =
    let module Hash = (val Digestif.module_of_hash' (hash :> Digestif.hash')) in
    Hash.digest_size
  in
  let cfg = Pate.config ~cachesize ~ref_length identify in
  (cfg, digest)

let setup_carton_config =
  let open Term in
  const setup_carton_config $ algorithm $ cachesize $ setup_signature

let term =
  let open Term in
  const run $ setup_logs $ setup_carton_config $ ipv4 $ ipv4_gateway $ port

let cmd =
  let info = Cmd.info "immuable" in
  Cmd.v info term

let () = Cmd.(exit @@ eval cmd)
