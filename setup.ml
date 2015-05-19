(* OASIS_START *)
(* OASIS_STOP *)

let () =
  InternalInstallPlugin.lib_hook :=
    fun (cs, bs, lib) ->
      match lib.OASISTypes.lib_findlib_name with
        | Some "core_kernel" ->
            (cs, bs, lib, [ "src/core_params.h"
                          ; "src/core_bigstring.h"
                          ; "src/jane_common.h"
                          ; "src/time_ns_stubs.h"
                          ; "src/timespec.h" ])
        | _ ->
            (cs, bs, lib, [])
;;

let () = setup ()
