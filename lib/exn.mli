open Never_returns

(* [sexp_of_t] uses a global table of sexp converters.  To register a converter for a new
   exception, add "with sexp" to its definition. If no suitable converter is found, the
   standard converter in [Printexc] will be used to generate an atomic S-expression. *)
type t = exn with sexp_of

include Pretty_printer.S with type t := t


(** Raised when finalization after an exception failed, too.
    The first exception argument is the one raised by the initial
    function, the second exception the one raised by the finalizer. *)
exception Finally of t * t

exception Reraised of string * t

(** Same as [raise], except that the backtrace is not recorded. *)
val raise_without_backtrace : t -> _

val reraise : t -> string -> _

(* Types with [format4] are hard to read, so here's an example.

   let foobar str =
     try
       ...
     with exn ->
       Exn.reraisef exn "Foobar is buggy on: %s" str ()
*)
val reraisef : t -> ('a, unit, string, unit -> _) format4 -> 'a

val to_string      : t -> string (* human-readable, multi-lines *)
val to_string_mach : t -> string (* machine format, single-line *)

(** Executes [f] and afterwards executes [finally], whether [f] throws an exception or
    not.
*)
val protectx : f:('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a

(** [handle_uncaught ~exit f] catches an exception escaping [f] and prints an error
    message to stderr.  Exits with return code 1 if [exit] is [true].  Otherwise returns
    unit.
*)
val handle_uncaught : exit:bool -> (unit -> unit) -> unit

(** [handle_uncaught_and_exit f] returns [f ()], unless that raises, in which case it
    prints the exception and exits nonzero. *)
val handle_uncaught_and_exit : (unit -> 'a) -> 'a

(* Traces exceptions passing through.  Useful because in practice backtraces still don't
   seem to work.

   Ex:
   let rogue_function () = if Random.bool () then failwith "foo" else 3
   let traced_function () = Exn.reraise_uncaught "rogue_function" rogue_function
   traced_function ();;

   : Program died with Reraised("rogue_function", Failure "foo")
*)
val reraise_uncaught : string -> (unit -> 'a) -> 'a

(** [Printexc.get_backtrace] *)
val backtrace : unit -> string
