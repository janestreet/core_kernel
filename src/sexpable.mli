(** New code should use the [@@deriving sexp] syntax directly.  These module types
    (S, S1, S2, and S3) are exported for backwards compatibility only. **)
module type S  = sig type              t [@@deriving sexp] end
module type S1 = sig type 'a           t [@@deriving sexp] end
module type S2 = sig type ('a, 'b)     t [@@deriving sexp] end
module type S3 = sig type ('a, 'b, 'c) t [@@deriving sexp] end

(** For when you want the sexp representation of one type to be the same as that for
    some other isomorphic type. *)
module Of_sexpable
    (Sexpable : S)
    (M : sig
       type t
       val to_sexpable : t -> Sexpable.t
       val of_sexpable : Sexpable.t -> t
     end)
  : S with type t := M.t

module Of_sexpable1
    (Sexpable : S1)
    (M : sig
       type 'a t
       val to_sexpable : 'a t -> 'a Sexpable.t
       val of_sexpable : 'a Sexpable.t -> 'a t
     end)
  : S1 with type 'a t := 'a M.t

module Of_sexpable2
    (Sexpable : S2)
    (M : sig
       type ('a, 'b) t
       val to_sexpable : ('a, 'b) t -> ('a, 'b) Sexpable.t
       val of_sexpable : ('a, 'b) Sexpable.t -> ('a, 'b) t
     end)
  : S2 with type ('a, 'b) t := ('a, 'b) M.t

module Of_stringable (M : Stringable.S) : S with type t := M.t
module To_stringable (M : S) : Stringable.S with type t := M.t


(** The following functors preserve stability: if applied to stable types with stable
    (de)serializations, they will produce stable types with stable (de)serializations.

    Note: In all cases, stability of the input (and therefore the output) depends on the
    semantics of all conversion functions (e.g. to_string, to_sexpable) not changing in
    the future.
*)
module Stable : sig
  module Of_sexpable   : sig module V1 : module type of Of_sexpable   end
  module Of_sexpable1  : sig module V1 : module type of Of_sexpable1  end
  module Of_sexpable2  : sig module V1 : module type of Of_sexpable2  end
  module Of_stringable : sig module V1 : module type of Of_stringable end
  module To_stringable : sig module V1 : module type of To_stringable end
end
