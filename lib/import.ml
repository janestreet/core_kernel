open Std_internal

let _squelch_unused_module_warning_ = ()

let concat = String.concat

let log message a sexp_of_a =
  Printf.eprintf "%s\n%!" (Sexp.to_string_hum (<:sexp_of< string * a >> (message, a)))
;;

let log_string msg = log msg () <:sexp_of< unit >>

let is_error = Result.is_error
let is_ok    = Result.is_ok

let does_fail f = is_error (Result.try_with f)

include Int.Replace_polymorphic_compare

module Poly = Polymorphic_compare

module Debug (M : sig end) = struct
  let check_invariant = ref true
  let show_messages   = ref true
  let debug invariant prefix =
    fun name ts arg sexp_of_arg sexp_of_result f ->
    if !show_messages then log (concat [ prefix; "."; name ]) arg sexp_of_arg;
    if !check_invariant then List.iter ts ~f:invariant;
    let result_or_exn = Result.try_with f in
    if !show_messages then
      log (concat [ prefix; "."; name; "-result" ]) result_or_exn
        <:sexp_of< (result, exn) Result.t >>;
    Result.ok_exn result_or_exn;
  ;;
end
