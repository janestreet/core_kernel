(* OASIS_START *)
(* OASIS_STOP *)

let () =
  InternalInstallPlugin.lib_hook :=
    fun (cs, bs, lib) ->
      match lib.OASISTypes.lib_findlib_name with
        | Some "core_kernel" ->
            (cs, bs, lib, [ "lib/core_params.h"
                          ; "lib/core_bigstring.h"
                          ; "lib/ocaml_utils.h"
                          ; "lib/jane_common.h"
                          ; "lib/timespec.h" ])
        | _ ->
            (cs, bs, lib, [])
;;

let () = setup ()
