open Sexplib
open Sexplib.Conv
module List = ListLabels
module String = StringLabels

let eprint message = Printf.eprintf "%s\n%!" message

let eprints message a sexp_of_a =
  eprint (Sexp.to_string_hum (<:sexp_of< string * a >> (message, a)));
;;

module Make (M : sig end) = struct
  let check_invariant = ref true
  let show_messages   = ref true
  let debug invariant ~module_name =
    fun name ts arg sexp_of_arg sexp_of_result f ->
    if !show_messages then
      eprints (String.concat ~sep:"" [ module_name; "."; name ]) arg sexp_of_arg;
    if !check_invariant then List.iter ts ~f:invariant;
    let result_or_exn = Result.try_with f in
    if !show_messages then
      eprints (String.concat ~sep:"" [ module_name; "."; name; "-result" ]) result_or_exn
        <:sexp_of< (result, exn) Result.t >>;
    Result.ok_exn result_or_exn;
  ;;
end
