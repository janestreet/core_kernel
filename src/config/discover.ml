open Base
module C = Configurator

let posix_timers_code = {|
#include <time.h>

int main()
{
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  return 0;
}
|}

type posix_timers =
  | Available of { need_lrt : bool }
  | Not_available

let () =
  C.main ~name:"core_kernel" (fun c ->
    let posix_timers =
      if C.c_test c posix_timers_code ~link_flags:["-lrt"] then
        Available { need_lrt = true }
      else if C.c_test c posix_timers_code then
        Available { need_lrt = false }
      else
        Not_available
    in

    let ocaml_vars =
      List.map
        (C.C_define.import c ~includes:["caml/config.h"]
           [ "ARCH_BIG_ENDIAN", Switch
           ; "ARCH_SIXTYFOUR" , Switch
           ])
        ~f:(fun (name, v) -> ("JSC_" ^ name, v))
    in

    C.C_define.gen_header_file c ~fname:"config.h"
      (("JSC_POSIX_TIMERS",
        match posix_timers with
        | Available _   -> Switch true
        | Not_available -> Switch false)

       :: ocaml_vars);

    let rt_flags : Sexp.t =
      match posix_timers with
      | Available { need_lrt = true } -> List [Atom "-lrt"]
      | _ -> List []
    in
    Stdio.Out_channel.write_all "rt-flags" ~data:(Sexp.to_string rt_flags))

