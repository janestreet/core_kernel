(* This module is deprecated.  Please use [Command]. *)

(* INRIA's original command-line parsing library. *)
include module type of Caml.Arg

type t = key * spec * doc

(** Like [align], except that the specification list is also sorted by key *)
val sort_and_align : (key * spec * doc) list -> (key * spec * doc) list
