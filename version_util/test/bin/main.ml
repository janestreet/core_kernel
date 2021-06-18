(* Force [Version_util] to be linked and make sure
   that that one can read/parse build_info. *)
let () = Printf.printf "ocaml_version is %s\n%!" Version_util.ocaml_version
let () = Printf.printf "build_info is %s\n%!" Version_util.build_info
let () = Printf.printf "version is %s\n%!" Version_util.version

let () =
  match Sys.backend_type with
  | Native ->
    if not Version_util.build_system_supports_version_util
    then failwith "Build system does not provide the version util support"
  | _ ->
    (* version util is not supported in javascript *)
    ()
;;
