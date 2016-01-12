open Sexplib
open Sexplib.Conv
module List = ListLabels
module String = StringLabels

let eprint message = Printf.eprintf "%s\n%!" message

let eprint_s sexp = eprint (Sexp.to_string_hum sexp)

let eprints message a sexp_of_a = eprint_s ([%sexp_of: string * a] (message, a))

let eprintf format = Printf.ksprintf eprint format

let failwiths = Error.failwiths

module Make () = struct
  let check_invariant = ref true
  let show_messages   = ref true

  let debug invariant ~module_name =
    fun name ts arg sexp_of_arg sexp_of_result f ->
      if !show_messages
      then eprints (String.concat ~sep:"" [ module_name; "."; name ]) arg sexp_of_arg;
      if !check_invariant
      then begin
        try List.iter ts ~f:invariant with exn ->
          failwiths "invariant pre-condition failed" (name, exn)
            [%sexp_of: string * exn]
      end;
      let result_or_exn = Result.try_with f in
      if !check_invariant
      then begin
        try List.iter ts ~f:invariant with exn ->
          failwiths "invariant post-condition failed" (name, exn)
            [%sexp_of: string * exn]
      end;
      if !show_messages
      then eprints (String.concat ~sep:"" [ module_name; "."; name; "-result" ])
             result_or_exn [%sexp_of: (result, exn) Result.t];
      Result.ok_exn result_or_exn;
  ;;
end
