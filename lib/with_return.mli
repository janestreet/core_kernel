(** This is [include]'d and documented in {! module: Common}.  It is defined here to avoid
    circular dependencies. *)

type 'a return = private { return : 'b. 'a -> 'b }

val with_return        : ('a return -> 'a  ) -> 'a

(** Note that [with_return_option] allocates ~5 words more than equivalent [with_return]
    call *)
val with_return_option : ('a return -> unit) -> 'a option
