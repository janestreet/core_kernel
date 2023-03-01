(** [Reversed_list] is constructed the same way as a list, but it needs to be reversed
    before it can be used. This is helpful when building up a list in reverse order to
    force reversal before use. *)
type 'a t =
  | []
  | ( :: ) of 'a * 'a t
[@@deriving equal]

(** The API of [Reversed_list] is purposely minimal to encourage destructing the list near
    the point of construction. Callers that are motivated to extend this API to avoid the
    overhead of reversal may be better off using a queue. *)

(** [of_list_rev] reverses the input list. *)
val of_list_rev : 'a list -> 'a t

val rev : 'a t -> 'a list
val rev_append : 'a t -> 'a list -> 'a list
val rev_map : 'a t -> f:('a -> 'b) -> 'b list
val rev_filter_map : 'a t -> f:('a -> 'b option) -> 'b list
val is_empty : 'a t -> bool
val length : 'a t -> int

(** Renders sexps without reversing the list. E.g. [1::2] is represented as [(1 2)].
    [of_sexp] and other derivations are not supported because [Reversed_list] is meant to
    be a more ephemeral type and [of_sexp] is only provided for printing convenience,
    e.g., for expect tests. Callers that are motivated to add derivations because they
    want to use [Reversed_list] as part of a type may be better off defining a custom type
    with a more meaningful name that conveys what the ordering represents instead of a
    generic "reversed list."  *)
module With_sexp_of : sig
  type nonrec 'a t = 'a t [@@deriving sexp_of]
end

(** Renders sexps after reversing the list. E.g. [1::2] is represented as [(2 1)]. See
    [With_sexp_of] for why only [sexp_of] is provided. *)
module With_rev_sexp_of : sig
  type nonrec 'a t = 'a t [@@deriving sexp_of]
end
