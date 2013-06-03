(* OASIS_START *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
open OASISTypes;;
(* OASIS_STOP *)

let () =
  InternalInstallPlugin.lib_hook :=
    fun (cs, bs, lib) ->
      match lib.OASISTypes.lib_findlib_name with
        | Some "core_kernel" ->
            (cs, bs, lib, ["lib/core_params.h"])
        | _ ->
            (cs, bs, lib, [])
;;

let () = setup ()
