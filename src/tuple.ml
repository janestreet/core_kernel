open! Import

module type T = sig type t end

module Make (T1 : T) (T2 : T) = struct type t = T1.t * T2.t end

module T2 = struct
  type ('a, 'b) t = 'a * 'b [@@deriving sexp, typerep]

  let create a b = (a, b)

  let curry f = (); fun a b -> f (a, b)

  let uncurry f = (); fun (a,b) -> f a b

  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"

  let map1 ~f (x,y) = (f x, y)
  let map2 ~f (x,y) = (x, f y)
  
  let mapn (x, y) = (f x, f y)
  
  let map_fst (x, y) ~f = (f x,   y)
  let map_snd (x, y) ~f = (  x, f y)

  let compare ~cmp1 ~cmp2 =
    fun (x, y) (x', y') ->
      match cmp1 x x' with
      | 0 -> cmp2 y y'
      | i -> i
  ;;

  let equal ~eq1 ~eq2 (x, y) (x', y') = eq1 x x' && eq2 y y'

  let swap (a, b) = (b, a)

  include Comparator.Derived2(struct
      type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of]
      let compare cmp1 cmp2 = compare ~cmp1 ~cmp2
    end)
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving sexp, typerep]

  let create a b c = (a, b, c)

  let curry f = (); fun a b c -> f (a,b,c)

  let uncurry f = (); fun (a,b,c) -> f a b c

  let map1 ~f (x,y,z) = (f x, y, z)
  let map2 ~f (x,y,z) = (x, f y, z)
  let map3 ~f (x,y,z) = (x, y, f z)

  let map_fst (x,y,z) ~f = (f x,   y,   z)
  let map_snd (x,y,z) ~f = (  x, f y,   z)
  let map_trd (x,y,z) ~f = (  x,   y, f z)

  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  (* There's no %field2....*)
  let get3 (_, _, a) = a

  (* lexicographic comparison  *)
  let compare ~cmp1 ~cmp2 ~cmp3 =
    fun (x, y, z) (x', y', z') ->
      let c1 = cmp1 x x' in
      if c1 <> 0 then c1 else
        let c2 = cmp2 y y' in
        if c2 <> 0 then c2 else
          cmp3 z z'
  ;;

  let equal ~eq1 ~eq2 ~eq3 (x, y, z) (x', y', z') = eq1 x x' && eq2 y y' && eq3 z z'

end

module type Comparable_sexpable = sig
  type t [@@deriving sexp]
  include Comparable.S with type t := t
end

module type Hashable_sexpable = sig
  type t [@@deriving sexp]
  include Hashable.S with type t := t
end

module type Hasher_sexpable = sig
  type t [@@deriving compare, hash, sexp]
end

module Sexpable (S1 : Sexpable.S) (S2 : Sexpable.S) = struct
  type t = S1.t * S2.t [@@deriving sexp]
end

module Binable (B1 : Binable.S) (B2 : Binable.S) = struct
  type t = B1.t * B2.t [@@deriving bin_io]
end

module Comparable (S1 : Comparable_sexpable) (S2 : Comparable_sexpable) = struct
  module T = struct
    include Sexpable (S1) (S2)

    let compare (s1, s2) (s1', s2') =
      match S1.compare s1 s1' with
      | 0 -> S2.compare s2 s2'
      | x -> x

  end

  include T
  include Comparable.Make (T)
end

module Hasher
    (H1 : Hasher_sexpable)
    (H2 : Hasher_sexpable) = struct
  module T = struct type t = H1.t * H2.t [@@deriving compare, hash, sexp] end
  include T
  include Hashable.Make(T)
end

module Hasher_sexpable_of_hashable_sexpable (S : Hashable_sexpable)
  : Hasher_sexpable with type t = S.t = struct
  include S
  let hash_fold_t state t = hash_fold_int state (hash t)
end

module Hashable_t
    (S1 : Hashable_sexpable)
    (S2 : Hashable_sexpable)
  = Hasher
    (Hasher_sexpable_of_hashable_sexpable (S1))
    (Hasher_sexpable_of_hashable_sexpable (S2))

module Hashable = Hashable_t
