(* Force [Js_of_ocaml_extended.Version_util] to be linked and make sure
   that that one can read/parse build_info. *)
let () = Printf.printf "ocaml_version is %s\n%!" Core_kernel.Version_util.ocaml_version
