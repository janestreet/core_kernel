(** Increment a counter whenever [caml_modify] is called.

    This library wraps caml_modify at the C level, and should only be used in testing
    code.
*)

(** [count ()] returns the number of times [caml_modify] has been called since the last
    call to {!reset}. *)
external count : unit -> int = "check_caml_modify_count" "noalloc"

(** [reset ()] reset the counter to [0]. *)
external reset : unit -> unit = "check_caml_modify_reset" "noalloc"
