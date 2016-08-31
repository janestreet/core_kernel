#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"core_kernel"
  [ oasis_lib "core_kernel"
  ; file "META" ~section:"lib"
  ; file "src/time_ns_stubs.h" ~section:"lib"
  ]
