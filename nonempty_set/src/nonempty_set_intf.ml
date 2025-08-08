open! Core

(** Roughly speaking, `Nonempty_set` is to `Set` as `Nonempty_list` is to `List`.

    A `('a, 'cw) Nonempty_set.t` is a `('a, 'cw) Set.t` which is guaranteed to not be
    empty. This invariant allows us to provide some additional type-safe operations, like
    [reduce], where the equivalent operation over a `Set` would be potentially
    exn-raising. *)

module type Nonempty_set = sig
  type (!'a, !'b) t

  val add : ('a, 'b) t -> 'a -> ('a, 'b) t
  val remove : ('a, 'b) t -> 'a -> ('a, 'b) t option
  val inter : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t option
  val singleton : ('a, 'cmp) Comparator.Module.t -> 'a -> ('a, 'cmp) t
  val reduce : ('a, _) t -> map:('a -> 'b) -> f:('b -> 'b -> 'b) -> 'b
  val fold : ('a, _) t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val iter : ('a, _) t -> f:('a -> unit) -> unit
  val of_list : ('a, 'cmp) Comparator.Module.t -> 'a list -> ('a, 'cmp) t option
  val of_list_exn : ('a, 'cmp) Comparator.Module.t -> 'a list -> ('a, 'cmp) t

  val of_nonempty_list
    :  ('a, 'cmp) Comparator.Module.t
    -> 'a Nonempty_list.t
    -> ('a, 'cmp) t

  val to_list : ('a, _) t -> 'a list
  val to_nonempty_list : ('a, _) t -> 'a Nonempty_list.t
  val map : ('b, 'cmp) Comparator.Module.t -> ('a, _) t -> f:('a -> 'b) -> ('b, 'cmp) t
  val of_set : ('a, 'b) Set.t -> ('a, 'b) t option
  val of_set_exn : ('a, 'b) Set.t -> ('a, 'b) t
  val of_set_add : ('a, 'b) Set.t -> 'a -> ('a, 'b) t
  val to_set : ('a, 'b) t -> ('a, 'b) Set.t
  val to_set_remove : ('a, 'b) t -> 'a -> ('a, 'b) Set.t
  val to_set_inter : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) Set.t

  (** [union t1 t2] returns the union of the two sets. [O(length t1 + length t2)]. *)
  val union : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  val union_set : ('a, 'b) t -> ('a, 'b) Set.t -> ('a, 'b) t

  (** [union t list] returns the union of [t] and all the sets in [list].
      [O(max(List.length list, n log n))], where [n] is the sum of sizes of the input
      sets. *)
  val union_list : ('a, 'cmp) t Nonempty_list.t -> ('a, 'cmp) t

  val union_set_list : ('a, 'cmp) t -> ('a, 'cmp) Set.t list -> ('a, 'cmp) t
  val diff : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) Set.t
  val mem : ('a, _) t -> 'a -> bool
  val length : (_, _) t -> int

  val%template equal : ('a, 'b) t -> ('a, 'b) t -> bool [@@mode m = (global, local)]

  val is_subset : ('a, 'b) t -> of_:('a, 'b) t -> bool
  val max_elt : ('a, _) t -> 'a
  val min_elt : ('a, _) t -> 'a
  val choose : ('a, _) t -> 'a

  include For_deriving.S_serializable with type ('a, 'b) t := ('a, 'b) t
  include For_deriving.S_unstable with type ('a, 'b) t := ('a, 'b) t

  module M (Elt : sig
      type t
      type comparator_witness
    end) : sig
    type nonrec t = (Elt.t, Elt.comparator_witness) t
  end

  module Stable : sig
    module V1 : sig
      type nonrec (!'a, !'b) t = ('a, 'b) t

      module M (Elt : sig
          type t
          type comparator_witness
        end) : sig
        type nonrec t = (Elt.t, Elt.comparator_witness) t
      end

      include For_deriving.S_stable with type ('a, 'b) t := ('a, 'b) t
      include For_deriving.S_serializable with type ('a, 'b) t := ('a, 'b) t
    end
  end
end
