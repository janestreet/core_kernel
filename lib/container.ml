(* This file has generic signatures for container data structures, with standard functions
   (iter, fold, exists, for_all, ...) that one would expect to find in any container.  The
   idea is to include [Container.S0] or [Container.S1] in the signature for every
   container-like data structure (Array, List, String, ...) to ensure a consistent
   interface. *)

open T
open With_return
open Perms.Export

module type T = sig
  type 'a t
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  (* The [iter] argument to [Container.Make] says how to implement the container's [iter]
     function.  [`Define_using_fold] means to define [iter t ~f = fold t ~init:() ~f:(fun
     () a -> f a)].

     [`Custom] overrides the default implementation, presumably with something more
     efficient.  Several other functions returned by [Container.Make] are defined in terms
     of [iter], so passing in a more efficient [iter] will improve their efficiency as
     well.
  *)
  val iter : [ `Define_using_fold
             | `Custom of ('a t -> f:('a -> unit) -> unit)
             ]
end

let fold_count fold t ~f = fold t ~init:0 ~f:(fun n a -> if f a then n + 1 else n)
let fold_sum (type a) (module M : Commutative_group.S with type t = a) fold t ~f =
  fold t ~init:M.zero ~f:(fun n a -> M.(+) n (f a))
;;

let fold_min fold t ~cmp =
  fold t ~init:None ~f:(fun acc elt ->
    match acc with
    | None -> Some elt
    | Some min -> if cmp min elt > 0 then Some elt else acc)
;;

let fold_max fold t ~cmp =
  fold t ~init:None ~f:(fun acc elt ->
    match acc with
    | None -> Some elt
    | Some max -> if cmp max elt < 0 then Some elt else acc)
;;

(** The idiom for using [Container.Make] is to bind the resulting module and to explicitly
    import each of the functions that one wants:

    {[
      module C = Container.Make (struct ... end)
      let count    = C.count
      let exists   = C.exists
      let find     = C.find
      ...
    ]}

    This is preferable to:

    {[
      include Container.Make (struct ... end)
    ]}

    because the [include] makes it to easy to shadow specialized implementations of
    container functions ([length] being a common one).
*)
module Make (T : T) = struct
  open T

  let fold = fold

  let count t ~f = fold_count fold t ~f
  let sum m t ~f = fold_sum m fold t ~f

  let iter_via_fold t ~f = fold t ~init:() ~f:(fun () a -> f a)

  let iter =
    match T.iter with
    | `Custom iter -> iter
    | `Define_using_fold -> iter_via_fold

  let length c = fold c ~init:0 ~f:(fun acc _ -> acc + 1)

  let is_empty c =
    with_return (fun r ->
      iter c ~f:(fun _ -> r.return false);
      true)
  ;;

  let exists c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return true);
      false)
  ;;

  let mem ?(equal = (=)) t a = exists t ~f:(equal a)

  let for_all c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if not (f x) then r.return false);
      true)
  ;;

  let find_map t ~f =
    with_return (fun r ->
      iter t ~f:(fun x -> match f x with None -> () | Some _ as res -> r.return res);
      None)
  ;;

  let find c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return (Some x));
      None)
  ;;

  let to_list c = List.rev (fold c ~init:[] ~f:(fun acc x -> x :: acc))

  let to_array c = Array.of_list (to_list c)

  let min_elt t ~cmp = fold_min fold t ~cmp

  let max_elt t ~cmp = fold_max fold t ~cmp
end

(* Signature for monomorphic container, e.g., string *)
module type S0 = sig
  type t
  type elt

  (** Checks whether the provided element is there using the default equality test, using
      the provided [equal] function if it is not *)
  val mem : ?equal:(elt -> elt -> bool) -> t -> elt -> bool

  val length   : t -> int

  val is_empty : t -> bool

  val iter     : t -> f:(elt -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]  *)
  val fold     : t -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists   : t -> f:(elt -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all  : t -> f:(elt -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count    : t -> f:(elt -> bool) -> int

  (** Returns the sum of [f i] for i in the container *)
  val sum
    : (module Commutative_group.S with type t = 'sum)
    -> t -> f:(elt -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find     : t -> f:(elt -> bool) -> elt option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : t -> f:(elt -> 'a option) -> 'a option

  val to_list  : t -> elt list
  val to_array : t -> elt array

  (** Returns a min (resp max) element from the collection using the provided [cmp]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same
      complexity as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : t -> cmp:(elt -> elt -> int) -> elt option
  val max_elt : t -> cmp:(elt -> elt -> int) -> elt option
end

module type S0_phantom = sig
  type elt
  type 'a t

  (** Checks whether the provided element is there using the default equality test, using
      the provided [equal] function if it is not *)
  val mem : ?equal:(elt -> elt -> bool) -> _ t -> elt -> bool

  val length   : _ t -> int

  val is_empty : _ t -> bool

  val iter     : _ t -> f:(elt -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]  *)
  val fold     : _ t -> init:'accum -> f:('accum -> elt -> 'accum) -> 'accum

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists   : _ t -> f:(elt -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all  : _ t -> f:(elt -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count    : _ t -> f:(elt -> bool) -> int

  (** Returns the sum of [f i] for i in the container *)
  val sum
    : (module Commutative_group.S with type t = 'sum)
    -> _ t -> f:(elt -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find     : _ t -> f:(elt -> bool) -> elt option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : _ t -> f:(elt -> 'a option) -> 'a option

  val to_list  : _ t -> elt list
  val to_array : _ t -> elt array

  (** Returns a min (resp max) element from the collection using the provided [cmp]
      function, or [None] if the collection is empty.  In case of a tie, the first element
      encountered while traversing the collection is returned. *)
  val min_elt : _ t -> cmp:(elt -> elt -> int) -> elt option
  val max_elt : _ t -> cmp:(elt -> elt -> int) -> elt option
end

(* Signature for polymorphic container, e.g., 'a list or 'a array *)
module type S1 = sig
  type 'a t

  (** Checks whether the provided element is there, using polymorphic compare if [equal]
      is not provided  *)
  val mem : ?equal:('a -> 'a -> bool) -> 'a t -> 'a -> bool

  val length   : 'a t -> int

  val is_empty : 'a t -> bool

  val iter     : 'a t -> f:('a -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]  *)
  val fold     : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists   : 'a t -> f:('a -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all  : 'a t -> f:('a -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count    : 'a t -> f:('a -> bool) -> int

  (** Returns the sum of [f i] for i in the container *)
  val sum
    : (module Commutative_group.S with type t = 'sum)
    -> 'a t -> f:('a -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find     : 'a t -> f:('a -> bool) -> 'a option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : 'a t -> f:('a -> 'b option) -> 'b option

  val to_list  : 'a t -> 'a list
  val to_array : 'a t -> 'a array

  (** Returns a minimum (resp maximum) element from the collection using the provided
      [cmp] function, or [None] if the collection is empty. In case of a tie, the first
      element encountered while traversing the collection is returned. The implementation
      uses [fold] so it has the same complexity as [fold]. *)
  val min_elt : 'a t -> cmp:('a -> 'a -> int) -> 'a option
  val max_elt : 'a t -> cmp:('a -> 'a -> int) -> 'a option
end

module type S1_phantom_invariant = sig
  type ('a, 'phantom) t

  (** Checks whether the provided element is there, using polymorphic compare if [equal]
      is not provided  *)
  val mem : ?equal:('a -> 'a -> bool) -> ('a, _) t -> 'a -> bool

  val length   : (_, _) t -> int
  val is_empty : (_, _) t -> bool
  val iter     : ('a, _) t -> f:('a -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]  *)
  val fold     : ('a, _) t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists   : ('a, _) t -> f:('a -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all  : ('a, _) t -> f:('a -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count    : ('a, _) t -> f:('a -> bool) -> int

  (** Returns the sum of [f i] for i in the container *)
  val sum
    : (module Commutative_group.S with type t = 'sum)
    -> ('a, _) t -> f:('a -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find     : ('a, _) t -> f:('a -> bool) -> 'a option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : ('a, _) t -> f:('a -> 'b option) -> 'b option

  val to_list  : ('a, _) t -> 'a list
  val to_array : ('a, _) t -> 'a array

  (** Returns a min (resp max) element from the collection using the provided [cmp]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same complexity
      as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : ('a, _) t -> cmp:('a -> 'a -> int) -> 'a option
  val max_elt : ('a, _) t -> cmp:('a -> 'a -> int) -> 'a option
end

module type S1_phantom = sig
  type ('a, +'phantom) t
  include S1_phantom_invariant with type ('a, 'phantom) t := ('a, 'phantom) t
end

module type S1_phantom_permissions = sig
  type ('a, -'permissions) t

  (** Checks whether the provided element is there, using polymorphic compare if [equal]
      is not provided  *)
  val mem : ?equal:('a -> 'a -> bool) -> ('a, [> read ]) t -> 'a -> bool

  val length   : (_, [> read ]) t -> int
  val is_empty : (_, [> read ]) t -> bool
  val iter     : ('a, [> read ]) t -> f:('a -> unit) -> unit

  (** [fold t ~init ~f] returns [f (... f (f (f init e1) e2) e3 ...) en], where [e1..en]
      are the elements of [t]  *)
  val fold     : ('a, [> read ]) t -> init:'accum
    -> f:('accum -> 'a -> 'accum) -> 'accum

  (** Returns [true] if and only if there exists an element for which the provided
      function evaluates to [true].  This is a short-circuiting operation. *)
  val exists   : ('a, [> read ]) t -> f:('a -> bool) -> bool

  (** Returns [true] if and only if the provided function evaluates to [true] for all
      elements.  This is a short-circuiting operation. *)
  val for_all  : ('a, [> read ]) t -> f:('a -> bool) -> bool

  (** Returns the number of elements for which the provided function evaluates to true. *)
  val count    : ('a, [> read ]) t -> f:('a -> bool) -> int

  (** Returns the sum of [f i] for i in the container *)
  val sum
    : (module Commutative_group.S with type t = 'sum)
    -> ('a, [> read ]) t -> f:('a -> 'sum) -> 'sum

  (** Returns as an [option] the first element for which [f] evaluates to true. *)
  val find     : ('a, [> read ]) t -> f:('a -> bool) -> 'a option

  (** Returns the first evaluation of [f] that returns [Some], and returns [None] if there
      is no such element.  *)
  val find_map : ('a, [> read ]) t -> f:('a -> 'b option) -> 'b option

  val to_list  : ('a, [> read ]) t -> 'a list
  val to_array : ('a, [> read ]) t -> 'a array

  (** Returns a min (resp max) element from the collection using the provided [cmp]
      function. In case of a tie, the first element encountered while traversing the
      collection is returned. The implementation uses [fold] so it has the same complexity
      as [fold]. Returns [None] iff the collection is empty. *)
  val min_elt : ('a, [> read ]) t -> cmp:('a -> 'a -> int) -> 'a option
  val max_elt : ('a, [> read ]) t -> cmp:('a -> 'a -> int) -> 'a option
end

module type Generic = sig
  type 'a t
  type 'a elt
  val mem : ?equal:('a elt -> 'a elt -> bool) -> 'a t -> 'a elt -> bool
  val length   : _  t -> int
  val is_empty : _  t -> bool
  val iter     : 'a t -> f:('a elt -> unit) -> unit
  val fold     : 'a t -> init:'accum -> f:('accum -> 'a elt -> 'accum) -> 'accum
  val exists   : 'a t -> f:('a elt -> bool) -> bool
  val for_all  : 'a t -> f:('a elt -> bool) -> bool
  val count    : 'a t -> f:('a elt -> bool) -> int
  val sum
    : (module Commutative_group.S with type t = 'sum)
    -> 'a t -> f:('a elt -> 'sum) -> 'sum
  val find     : 'a t -> f:('a elt -> bool) -> 'a elt option
  val find_map : 'a t -> f:('a elt -> 'b option) -> 'b option
  val to_list  : 'a t -> 'a elt list
  val to_array : 'a t -> 'a elt array
  val min_elt  : 'a t -> cmp:('a elt -> 'a elt -> int) -> 'a elt option
  val max_elt  : 'a t -> cmp:('a elt -> 'a elt -> int) -> 'a elt option
end

module type Generic_phantom = sig
  type ('a, 'phantom) t
  type 'a elt
  val mem : ?equal:('a elt -> 'a elt -> bool) -> ('a, _) t -> 'a elt -> bool
  val length   : (_, _) t -> int
  val is_empty : (_, _) t -> bool
  val iter     : ('a, _) t -> f:('a elt -> unit) -> unit
  val fold     : ('a, _) t -> init:'accum -> f:('accum -> 'a elt -> 'accum) -> 'accum
  val exists   : ('a, _) t -> f:('a elt -> bool) -> bool
  val for_all  : ('a, _) t -> f:('a elt -> bool) -> bool
  val count    : ('a, _) t -> f:('a elt -> bool) -> int
  val sum
    : (module Commutative_group.S with type t = 'sum)
    -> ('a, _) t -> f:('a elt -> 'sum) -> 'sum
  val find     : ('a, _) t -> f:('a elt -> bool) -> 'a elt option
  val find_map : ('a, _) t -> f:('a elt -> 'b option) -> 'b option
  val to_list  : ('a, _) t -> 'a elt list
  val to_array : ('a, _) t -> 'a elt array
  val min_elt  : ('a, _) t -> cmp:('a elt -> 'a elt -> int) -> 'a elt option
  val max_elt  : ('a, _) t -> cmp:('a elt -> 'a elt -> int) -> 'a elt option
end

(* The following functors exist as a consistency check among all the various [S?]
   interfaces.  They ensure that each particular [S?] is an instance of a more generic
   signature. *)
module Check (T : T1) (Elt : T1)
  (M : Generic with type 'a t := 'a T.t with type 'a elt := 'a Elt.t) = struct end

module Check_S0 (M : S0) =
  Check (struct type 'a t = M.t end) (struct type 'a t = M.elt end) (M)

module Check_S0_phantom (M : S0_phantom) =
  Check (struct type 'a t = 'a M.t end) (struct type 'a t = M.elt end) (M)

module Check_S1 (M : S1) =
  Check (struct type 'a t = 'a M.t end) (struct type 'a t = 'a end) (M)

type phantom

module Check_S1_phantom (M : S1_phantom) =
  Check (struct type 'a t = ('a, phantom) M.t end) (struct type 'a t = 'a end) (M)

module Check_S1_phantom_invariant (M : S1_phantom_invariant) =
  Check (struct type 'a t = ('a, phantom) M.t end) (struct type 'a t = 'a end) (M)
