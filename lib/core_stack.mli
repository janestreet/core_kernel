(** A stack implemented with an array.  See {!Stack_intf} for documentation.

    The implementation will grow the array as necessary.  By default, it will not
    automatically shrink the array.  One can use [set_auto_shrink] to change
    auto-shrinking behavior.
*)

include Stack_intf.S

(** [set_auto_shrink t] controls whether [t]'s array will automatically shrink when the
    number of elements on the stack falls below 1/4 of the array size. *)
val set_auto_shrink : _ t -> bool -> unit
