@@ portable

(** [Reversed_list] is constructed the same way as a list, but it needs to be reversed
    before it can be used. This is helpful when building up a list in reverse order to
    force reversal before use. *)
type 'a t =
  | []
  | ( :: ) of 'a * 'a t
[@@deriving equal ~localize]

(** Type alias for use within submodules *)
type 'a reversed_list = 'a t

(** The API of [Reversed_list] is purposely minimal to encourage destructing the list near
    the point of construction. Callers that are motivated to extend this API to avoid the
    overhead of reversal may be better off using a queue. *)

(** [of_list_rev] reverses the input list. *)
val%template of_list_rev : 'a list @ m -> 'a t @ m
[@@alloc __ @ m = (stack_local, heap_global)]

val%template rev : 'a t @ m -> 'a list @ m [@@alloc __ @ m = (stack_local, heap_global)]

val rev_append : 'a t -> 'a list -> 'a list
val rev_map : 'a t -> f:('a -> 'b) -> 'b list
val rev_filter_map : 'a t -> f:('a -> 'b option) -> 'b list
val is_empty : 'a t -> bool
val length : 'a t -> int

(** Submodule for only bringing the constructors into scope (primarily for [::]). *)
module O : sig
  type nonrec 'a t = 'a t =
    | []
    | ( :: ) of 'a * 'a t
end

(** Renders sexps without reversing the list. E.g. [1::2] is represented as [(1 2)].
    [of_sexp] and other derivations are not supported because [Reversed_list] is meant to
    be a more ephemeral type and [sexp_of] is only provided for printing convenience,
    e.g., for expect tests. Callers that are motivated to add derivations because they
    want to use [Reversed_list] as part of a type may be better off defining a custom type
    with a more meaningful name that conveys what the ordering represents instead of a
    generic "reversed list." *)
module With_sexp_of : sig
  type nonrec 'a t = 'a t [@@deriving sexp_of]
end

(** Renders sexps after reversing the list. E.g. [1::2] is represented as [(2 1)]. See
    [With_sexp_of] for why only [sexp_of] is provided. *)
module With_rev_sexp_of : sig
  type nonrec 'a t = 'a t [@@deriving sexp_of]
end

(** Functions that operate between [Reversed_list.t] and [Nonempty_list.t] *)

(** Non-empty version of [Reversed_list.t] *)
module Nonempty : sig
  type 'a t = ( :: ) of 'a * 'a reversed_list

  val cons : 'a -> 'a t -> 'a t
  val to_rev_list : 'a t -> 'a reversed_list
  val rev : 'a t -> 'a Core.Nonempty_list.t
  val rev_append : 'a t -> 'a list -> 'a Core.Nonempty_list.t
  val rev_map : 'a t -> f:local_ ('a -> 'b) -> 'b Core.Nonempty_list.t
  val rev_mapi : 'a t -> f:local_ (int -> 'a -> 'b) -> 'b Core.Nonempty_list.t

  (** Renders sexps without reversing the list. E.g. [1::2] is represented as [(1 2)]. *)
  module With_sexp_of : sig
    type nonrec 'a t = 'a t [@@deriving sexp_of]
  end

  (** Renders sexps after reversing the list. E.g. [1::2] is represented as [(2 1)]. *)
  module With_rev_sexp_of : sig
    type nonrec 'a t = 'a t [@@deriving sexp_of]
  end
end

(** [of_nonempty t] converts a nonempty list to a nonempty reversed list. *)
val of_nonempty : 'a Core.Nonempty_list.t -> 'a Nonempty.t

(** [rev_append_to_nonempty xs acc] reverses [xs] and prepends it to the nonempty list
    [acc]. *)
val rev_append_to_nonempty : 'a t -> 'a Core.Nonempty_list.t -> 'a Core.Nonempty_list.t
