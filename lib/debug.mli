(** Utilities for printing debug messages. *)

open Sexplib

(** [eprint message] prints to stderr [message], followed by a newline and flush.  This is
    the same as [prerr_endline]. *)
val eprint : string -> unit

(** [eprints message a sexp_of_a] prints to stderr [message] and [a] as a sexp, followed
    by a newline and flush. *)
val eprints : string -> 'a -> ('a -> Sexp.t) -> unit

(** [eprintf message arg1 ... argn] prints to stderr [message], with sprintf-style format
    characters instantiated, followed by a newline and flush. *)
val eprintf : ('r, unit, string, unit) format4 -> 'r

(** [Debug.Make] produces a [debug] function used to wrap a function to display arguments
    before calling and display results after returning.  Intended usage is:

    {[
      module Foo = struct
        type t = ...
        let invariant = ...
        let bar t x y : Result.t = ...
      end
      module Foo_debug = struct
        open Foo
        include Debug.Make ()
        let debug x = debug invariant ~module_name:"Foo" x
        let bar t x y =
          debug "bar" [t] (t, x, y) <:sexp_of< t * X.t * Y.t >> <:sexp_of< Result.t >>
            (fun () -> bar t x y)
      end
    ]}
*)
module Make (M : sig end) : sig
  (* Whether the invariants are called on each invocation *)
  val check_invariant : bool ref

  (* If true, you get a message on stderr every time [debug] is called *)
  val show_messages   : bool ref

  (* We avoid labels so that the applications are more concise -- see example above *)
  val debug
    :  't Invariant.t
    -> module_name:string             (* appears on messages *)
    -> (string                        (* name of function [f], also appears on messages *)
        -> 't list (* args of type [t], to have invariant checked iff [check_invariant] *)
        -> 'args   (* arguments to function we're debugging *)
        -> ('args -> Sexp.t)
        -> ('result -> Sexp.t)
        -> (unit -> 'result)          (* should call [f] with ['args], exn's re-raised *)
        -> 'result
       )
end
