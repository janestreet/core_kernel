(** Substring type based on [Bigarray], for use in I/O and C-bindings *)

include Make_substring.S with type base = Bigstring.t
