#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"core_kernel"
  [ oasis_lib "core_kernel"
  ; file "META" ~section:"lib"
  ; file "include/core_params.h" ~section:"lib"
  ; file "include/jane_common.h" ~section:"lib"
  ; file "include/ocaml_utils.h" ~section:"lib"
  ; file "include/unix_utils.h" ~section:"lib"
  ; file "src/time_ns_stubs.h" ~section:"lib"
  ; file "_build/namespace_wrappers/result_lib.cmi" ~section:"lib"
  ]
