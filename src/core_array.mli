open Perms.Export

type 'a t = 'a array [@@deriving bin_io, compare, sexp, typerep]

include Binary_searchable.S1 with type 'a t := 'a t

(** Note: [Array.length] is not constant for a given array, as one can reduce it with
    [Array.truncate] *)
include Container.S1 with type 'a t := 'a t

include Invariant.S1 with type 'a t := 'a t

(** Maximum length of a normal array.  The maximum length of a float array is
    [max_length/2] on 32-bit machines and [max_length] on 64-bit machines. *)
val max_length : int

(** [Array.get a n] returns the element number [n] of array [a].
    The first element has number 0.
    The last element has number [Array.length a - 1].
    You can also write [a.(n)] instead of [Array.get a n].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [(Array.length a - 1)]. *)
external get : 'a t -> int -> 'a = "%array_safe_get"

(** [Array.set a n x] modifies array [a] in place, replacing
    element number [n] with [x].
    You can also write [a.(n) <- x] instead of [Array.set a n x].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [Array.length a - 1]. *)
external set : 'a t -> int -> 'a -> unit = "%array_safe_set"

(** Unsafe version of [get].  Can cause arbitrary behavior when used for an out-of-bounds
    array access *)
external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"

(** Unsafe version of [set].  Can cause arbitrary behavior when used for an out-of-bounds
    array access *)
external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"

(** [create ~len x] creates an array of length [len] with the value [x] populated in
    each element *)
val create : len:int -> 'a -> 'a t

(** [init n ~f] creates an array of length [n] where the [i]th element is initialized
    with [f i] (starting at zero) *)
val init : int -> f:(int -> 'a) -> 'a t

(** [Array.make_matrix dimx dimy e] returns a two-dimensional array
    (an array of arrays) with first dimension [dimx] and
    second dimension [dimy]. All the elements of this new matrix
    are initially physically equal to [e].
    The element ([x,y]) of a matrix [m] is accessed
    with the notation [m.(x).(y)].

    Raise [Invalid_argument] if [dimx] or [dimy] is negative or
    greater than [Sys.max_array_length].
    If the value of [e] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2]. *)
val make_matrix : dimx:int -> dimy:int -> 'a -> 'a t t

(** [Array.append v1 v2] returns a fresh array containing the
    concatenation of the arrays [v1] and [v2]. *)
val append : 'a t -> 'a t -> 'a t

(** Like [Array.append], but concatenates a list of arrays. *)
val concat : 'a t list -> 'a t

(** [Array.copy a] returns a copy of [a], that is, a fresh array
    containing the same elements as [a]. *)
val copy : 'a t -> 'a t

(** [Array.fill a ofs len x] modifies the array [a] in place,
    storing [x] in elements number [ofs] to [ofs + len - 1].

    Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not
    designate a valid subarray of [a]. *)
val fill : 'a t -> pos:int -> len:int -> 'a -> unit

(** [Array.blit v1 o1 v2 o2 len] copies [len] elements from array [v1], starting at
    element number [o1], to array [v2], starting at element number [o2].  It works
    correctly even if [v1] and [v2] are the same array, and the source and destination
    chunks overlap.

    Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not designate a valid
    subarray of [v1], or if [o2] and [len] do not designate a valid subarray of [v2].

    [int_blit] and [float_blit] provide fast bound-checked blits for immediate
    data types.  The unsafe versions do not bound-check the arguments. *)
include Blit.S1 with type 'a t := 'a t

module Int : sig
  type nonrec t = int t [@@deriving bin_io, compare, sexp]

  include Blit.S with type t := t

  external unsafe_blit
    : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
    = "core_array_unsafe_int_blit" "noalloc"
end

module Float : sig
  type nonrec t = float t [@@deriving bin_io, compare, sexp]

  include Blit.S with type t := t

  external unsafe_blit
    : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
    = "core_array_unsafe_float_blit" "noalloc"
end

(** [Array.of_list l] returns a fresh array containing the elements of [l]. *)
val of_list : 'a list -> 'a t

(** [Array.map ~f a] applies function [f] to all the elements of [a],
    and builds an array with the results returned by [f]:
    [[| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |]]. *)
val map : f:('a -> 'b) -> 'a t -> 'b t

(** Like {!Array.iter}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit

(** Like {!Array.map}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t

val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

(** [Array.fold_right f a ~init] computes
    [f a.(0) (f a.(1) ( ... (f a.(n-1) init) ...))],
    where [n] is the length of the array [a]. *)
val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

(** All sort functions in this module sort in increasing order by default.  *)

(** [sort] uses constant heap space. [stable_sort] uses linear heap space.

    To sort only part of the array, specify [pos] to be the index to start sorting from
    and [len] indicating how many elements to sort.
*)
val sort : ?pos:int -> ?len:int -> 'a t -> cmp:('a -> 'a -> int) -> unit
val stable_sort : 'a t -> cmp:('a -> 'a -> int) -> unit

val is_sorted : 'a t -> cmp:('a -> 'a -> int) -> bool

(** [is_sorted_strictly xs ~cmp] iff [is_sorted xs ~cmp] and no two consecutive elements
   in [xs] are equal according to [cmp] *)
val is_sorted_strictly : 'a t -> cmp:('a -> 'a -> int) -> bool

(** Like [List.concat_map], [List.concat_mapi]. *)
val concat_map  : 'a t -> f:(       'a -> 'b array) -> 'b array
val concat_mapi : 'a t -> f:(int -> 'a -> 'b array) -> 'b array

val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t

val partitioni_tf : 'a t -> f:(int -> 'a -> bool) -> 'a t * 'a t

val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

(** [transpose] in the sense of a matrix transpose.  It returns [None] if the arrays are
    not all the same length. *)
val transpose     : 'a t t -> 'a t t option
val transpose_exn : 'a t t -> 'a t t

(** [normalize array index] returns a new index into the array such that if index is
    less than zero, the returned index will "wrap around" -- i.e. array.(normalize array
    (-1)) returns the last element of the array. *)
val normalize : 'a t -> int -> int

(** [slice array start stop] returns a fresh array including elements [array.(start)]
    through [array.(stop-1)] with the small tweak that the start and stop positions are
    normalized and a stop index of 0 means the same thing a stop index of [Array.length
    array].  In summary, it's mostly like the slicing in Python or Matlab.  One
    difference is that a stop value of 0 here is like not specifying a stop value in
    Python. *)
val slice : 'a t -> int -> int -> 'a t

(** Array access with [normalize]d index. *)
val nget : 'a t -> int -> 'a

(** Array modification with [normalize]d index. *)
val nset : 'a t -> int -> 'a -> unit

(** [filter_opt array] returns a new array where [None] entries are omitted and [Some x]
    entries are replaced with [x]. Note that this changes the index at which elements
    will appear. *)
val filter_opt : 'a option t -> 'a t

(** [filter_map ~f array] maps [f] over [array] and filters [None] out of the
    results. *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** Like [filter_map] but uses {!Array.mapi}. *)
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

(** Like [for_all], but passes the index as an argument. *)
val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool

(** Like [exists], but passes the index as an argument. *)
val existsi : 'a t -> f:(int -> 'a -> bool) -> bool

(** Like [count], but passes the index as an argument. *)
val counti : 'a t -> f:(int -> 'a -> bool) -> int

(** Functions with 2 suffix raise an exception if the lengths aren't the same. *)
val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit

val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val fold2_exn : 'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) -> 'c

(** [for_all2_exn t1 t2 ~f] fails if [length t1 <> length t2]. *)
val for_all2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

(** [exists2_exn t1 t2 ~f] fails if [length t1 <> length t2]. *)
val exists2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

(** [filter ~f array] removes the elements for which [f] returns false.  *)
val filter : f:('a -> bool) -> 'a t -> 'a t

(** Like [filter] except [f] also receives the index. *)
val filteri : f:(int -> 'a -> bool) -> 'a t -> 'a t

(** [swap arr i j] swaps the value at index [i] with that at index [j]. *)
val swap : 'a t -> int -> int -> unit

(** [rev_inplace t] reverses [t] in place *)
val rev_inplace : 'a t -> unit

(** [of_list_rev l] converts from list then reverses in place *)
val of_list_rev : 'a list -> 'a t

(** [of_list_map l ~f] is the same as [of_list (List.map l ~f)] *)
val of_list_map : 'a list -> f:('a -> 'b) -> 'b t

(** [of_list_rev_map l ~f] is the same as [rev_inplace (of_list_map l ~f)] *)
val of_list_rev_map : 'a list -> f:('a -> 'b) -> 'b t

(** [replace t i ~f] = [t.(i) <- f (t.(i))]. *)
val replace : 'a t -> int -> f:('a -> 'a) -> unit

(** modifies an array in place -- [ar.(i)] will be set to [f(ar.(i))] *)
val replace_all : 'a t -> f:('a -> 'a) -> unit

(** [find_exn f t] returns the first [a] in [t] for which [f t.(i)] is true.
    It raises [Not_found] if there is no such [a]. *)
val find_exn : 'a t -> f:('a -> bool) -> 'a

(** Returns the first evaluation of [f] that returns [Some].
    Raises [Not_found] if [f] always returns [None].  *)
val find_map_exn : 'a t -> f:('a -> 'b option) -> 'b

(** [findi t f] returns the first index [i] of [t] for which [f i t.(i)] is true *)
val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option

(** [findi_exn t f] returns the first index [i] of [t] for which [f i t.(i)] is
    true.  It raises [Not_found] if there is no such element. *)
val findi_exn : 'a t -> f:(int -> 'a -> bool) -> int * 'a

(** [find_mapi t f] is the like [find_map] but passes the index as an argument. *)
val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option

(** [find_mapi_exn] is the like [find_map_exn] but passes the index as an argument. *)
val find_mapi_exn : 'a t -> f:(int -> 'a -> 'b option) -> 'b

(** [find_consecutive_duplicate t ~equal] returns the first pair of consecutive elements
    [(a1, a2)] in [t] such that [equal a1 a2].  They are returned in the same order as
    they appear in [t]. *)
val find_consecutive_duplicate : 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option

(** [reduce f [a1; ...; an]] is [Some (f (... (f (f a1 a2) a3) ...) an)].
    Returns [None] on the empty array. *)
val reduce     : 'a t -> f:('a -> 'a -> 'a) -> 'a option
val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a

(** [permute ?random_state t] randomly permutes [t] in place.

    [permute] side affects [random_state] by repeated calls to [Random.State.int].
    If [random_state] is not supplied, [permute] uses [Random.State.default]. *)
val permute : ?random_state:Core_random.State.t -> 'a t -> unit

(** [zip] is like [List.zip], but for arrays. *)
val zip     : 'a t -> 'b t -> ('a * 'b) t option
val zip_exn : 'a t -> 'b t -> ('a * 'b) t

(** [unzip] is like [List.unzip], but for arrays. *)
val unzip : ('a * 'b) t -> 'a t * 'b t

(** [sorted_copy ar cmp] returns a shallow copy of [ar] that is sorted. Similar to
    List.sort *)
val sorted_copy : 'a t -> cmp:('a -> 'a -> int) -> 'a t

val last : 'a t -> 'a

(** [empty ()] creates an empty array *)
val empty : unit -> 'a t

val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool

(** [truncate t ~len] drops [length t - len] elements from the end of [t], changing [t]
    so that [length t = len] afterwards.  [truncate] raises if [len <= 0 || len > length
    t]. *)
val truncate : _ t -> len:int -> unit


(** [to_sequence t] converts [t] to a sequence. [t] is copied internally so that future
    modifications of [t] do not change the sequence. *)
val to_sequence : 'a t -> 'a Sequence.t

(** [to_sequence_mutable t] converts [t] to a sequence. [t] is shared with the sequence
    and modifications of [t] will result in modification of the sequence. *)
val to_sequence_mutable : 'a t -> 'a Sequence.t

(** The [Permissioned] module gives the ability to restrict permissions on an array, so
    you can give a function read-only access to an array, create an immutable array, etc.
*)
module Permissioned : sig
  (** The meaning of the ['perms] parameter is as usual (see the [Perms] module for more
      details) with the non-obvious difference that you don't need any permissions to
      extract the length of an array.  This was done for simplicity because some
      information about the length of an array can leak out even if you only have write
      permissions since you can catch out-of-bounds errors.
  *)
  type ('a, -'perms) t [@@deriving bin_io, compare, sexp]

  module Int : sig
    type nonrec -'perms t = (int, 'perms) t [@@deriving bin_io, compare, sexp]

    include Blit.S_permissions with type 'perms t := 'perms t

    external unsafe_blit
      : src:[> read] t -> src_pos:int -> dst:[> write] t -> dst_pos:int -> len:int -> unit
      = "core_array_unsafe_int_blit" "noalloc"
  end

  module Float : sig
    type nonrec -'perms t = (float, 'perms) t [@@deriving bin_io, compare, sexp]

    include Blit.S_permissions with type 'perms t := 'perms t

    external unsafe_blit
      : src:[> read] t -> src_pos:int -> dst:[> write] t -> dst_pos:int -> len:int -> unit
      = "core_array_unsafe_float_blit" "noalloc"
  end

  (** [of_array_id] and [to_array_id] return the same underlying array.  On the other
      hand, [to_array] (inherited from [Container.S1_permissions] below) makes a copy.

      To create a new (possibly immutable) copy of an array [a], use [copy (of_array_id
      a)].  More generally, any function that takes a (possibly mutable) [t] can be called
      on an array by calling [of_array_id] on it first.

      There is a conceptual type equality between ['a Array.t] and
      [('a, read_write) Array.Permissioned.t].  The reason for not exposing this as an
      actual type equality is that we also want:

      {ul
      {- The type equality ['a Array.t = 'a array] for interoperability with code which
      does not use Core.}
      {- The type [('a, 'perms) Array.Permissioned.t] to be abstract, so that the
      permission phantom type will have an effect.}
      }

      Since we don't control the definition of ['a array], this would require a type
      [('a, 'perms) Array.Permissioned.t] which is abstract, except that
      [('a, read_write) Array.Permissioned.t] is concrete, which is not possible.
  *)
  val of_array_id : 'a array -> ('a, [< read_write]) t
  val to_array_id : ('a, [> read_write]) t -> 'a array

  (** [to_sequence_immutable t] converts [t] to a sequence. Unlike [to_sequence],
      [to_sequence_immutable] does not need to copy [t] since it is immutable. *)
  val to_sequence_immutable : ('a, [> immutable]) t -> 'a Sequence.t

  include Container.        S1_permissions with type ('a, 'perms) t := ('a, 'perms) t
  include Blit.             S1_permissions with type ('a, 'perms) t := ('a, 'perms) t
  include Binary_searchable.S1_permissions with type ('a, 'perms) t := ('a, 'perms) t

  (** These functions are in [Container.S1_permissions], but they are re-exposed here so
      that their types can be changed to make them more permissive (see comment above). *)

  val length   : (_, _) t -> int
  val is_empty : (_, _) t -> bool

  (** counterparts of regular array functions above *)

  external get        : ('a, [> read] ) t -> int -> 'a         = "%array_safe_get"
  external set        : ('a, [> write]) t -> int -> 'a -> unit = "%array_safe_set"
  external unsafe_get : ('a, [> read] ) t -> int -> 'a         = "%array_unsafe_get"
  external unsafe_set : ('a, [> write]) t -> int -> 'a -> unit = "%array_unsafe_set"

  val create : len:int -> 'a -> ('a, [< _ perms]) t
  val init : int -> f:(int -> 'a) -> ('a, [< _ perms]) t
  val make_matrix
    :  dimx:int
    -> dimy:int
    -> 'a
    -> (('a, [< _ perms]) t, [< _ perms]) t
  val append : ('a, [> read]) t -> ('a, [> read]) t -> ('a, [< _ perms]) t
  val concat : ('a, [> read]) t list -> ('a, [< _ perms]) t
  val copy : ('a, [> read]) t -> ('a, [< _ perms]) t
  val fill : ('a, [> write]) t -> pos:int -> len:int -> 'a -> unit
  val of_list : 'a list -> ('a, [< _ perms]) t
  val map   : f:('a -> 'b)          -> ('a, [> read]) t -> ('b, [< _ perms]) t
  val iteri : f:(int -> 'a -> unit) -> ('a, [> read]) t -> unit
  val mapi  : f:(int -> 'a -> 'b)   -> ('a, [> read]) t -> ('b, [< _ perms]) t
  val foldi      : ('a, [> read]) t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b
  val fold_right : ('a, [> read]) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b
  val sort
    :  ?pos:int
    -> ?len:int
    -> ('a, [> read_write]) t
    -> cmp:('a -> 'a -> int)
    -> unit
  val stable_sort        : ('a, [> read_write]) t -> cmp:('a -> 'a -> int) -> unit
  val is_sorted          : ('a, [> read]      ) t -> cmp:('a -> 'a -> int) -> bool
  val is_sorted_strictly : ('a, [> read]      ) t -> cmp:('a -> 'a -> int) -> bool
  val concat_map
    :  ('a, [> read]) t
    -> f:('a -> ('b, [> read]) t)
    -> ('b, [< _ perms]) t
  val concat_mapi
    :  ('a, [> read]) t
    -> f:(int -> 'a -> ('b, [> read]) t)
    -> ('b, [< _ perms]) t
  val partition_tf
    :  ('a, [> read]) t
    -> f:('a -> bool)
    -> ('a, [< _ perms]) t * ('a, [< _ perms]) t
  val partitioni_tf
    :  ('a, [> read]) t
    -> f:(int -> 'a -> bool)
    -> ('a, [< _ perms]) t * ('a, [< _ perms]) t
  val cartesian_product : ('a, [> read]) t -> ('b, [> read]) t -> ('a * 'b, [< _ perms]) t
  val transpose     : (('a, [> read]) t, [> read]) t -> (('a, [< _ perms]) t, [< _ perms]) t option
  val transpose_exn : (('a, [> read]) t, [> read]) t -> (('a, [< _ perms]) t, [< _ perms]) t
  val normalize : (_, _) t -> int -> int
  val slice : ('a, [> read]) t -> int -> int -> ('a, [< _ perms]) t
  val nget : ('a, [> read] ) t -> int -> 'a
  val nset : ('a, [> write]) t -> int -> 'a -> unit
  val filter_opt  : ('a option, [> read]) t -> ('a, [< _ perms]) t
  val filter_map  : ('a, [> read]) t -> f:(       'a -> 'b option) -> ('b, [< _ perms]) t
  val filter_mapi : ('a, [> read]) t -> f:(int -> 'a -> 'b option) -> ('b, [< _ perms]) t

  val for_alli : ('a, [> read]) t -> f:(int -> 'a -> bool) -> bool
  val existsi  : ('a, [> read]) t -> f:(int -> 'a -> bool) -> bool
  val counti   : ('a, [> read]) t -> f:(int -> 'a -> bool) -> int

  val iter2_exn : ('a, [> read]) t -> ('b, [> read]) t -> f:('a -> 'b -> unit) -> unit
  val map2_exn
    :  ('a, [> read]) t
    -> ('b, [> read]) t
    -> f:('a -> 'b -> 'c)
    -> ('c, [< _ perms]) t
  val fold2_exn
    :  ('a, [> read]) t
    -> ('b, [> read]) t
    -> init:'c
    -> f:('c -> 'a -> 'b -> 'c)
    -> 'c
  val for_all2_exn
    :  ('a, [> read]) t
    -> ('b, [> read]) t
    -> f:('a -> 'b -> bool)
    -> bool
  val exists2_exn
    :  ('a, [> read]) t
    -> ('b, [> read]) t
    -> f:('a -> 'b -> bool)
    -> bool
  val filter  : f:('a -> bool)        -> ('a, [> read]) t -> ('a, [< _ perms]) t
  val filteri : f:(int -> 'a -> bool) -> ('a, [> read]) t -> ('a, [< _ perms]) t
  val swap : ('a, [> read_write]) t -> int -> int -> unit
  val rev_inplace : ('a, [> read_write]) t -> unit
  val of_list_rev     : 'a list -> ('a, [< _ perms]) t
  val of_list_map     : 'a list -> f:('a -> 'b) -> ('b, [< _ perms]) t
  val of_list_rev_map : 'a list -> f:('a -> 'b) -> ('b, [< _ perms]) t
  val replace     : ('a, [> read_write]) t -> int -> f:('a -> 'a) -> unit
  val replace_all : ('a, [> read_write]) t        -> f:('a -> 'a) -> unit
  val find_exn     : ('a, [> read]) t -> f:('a -> bool) -> 'a
  val find_map_exn : ('a, [> read]) t -> f:('a -> 'b option) -> 'b
  val findi     : ('a, [> read]) t -> f:(int -> 'a -> bool) -> (int * 'a) option
  val findi_exn : ('a, [> read]) t -> f:(int -> 'a -> bool) -> int * 'a
  val find_mapi     : ('a, [> read]) t -> f:(int -> 'a -> 'b option) -> 'b option
  val find_mapi_exn : ('a, [> read]) t -> f:(int -> 'a -> 'b option) -> 'b
  val find_consecutive_duplicate
    :  ('a, [> read]) t
    -> equal:('a -> 'a -> bool)
    -> ('a * 'a) option
  val reduce     : ('a, [> read]) t -> f:('a -> 'a -> 'a) -> 'a option
  val reduce_exn : ('a, [> read]) t -> f:('a -> 'a -> 'a) -> 'a
  val permute : ?random_state:Core_random.State.t -> ('a, [> read_write]) t -> unit
  val zip     : ('a, [> read]) t -> ('b, [> read]) t -> ('a * 'b, [< _ perms]) t option
  val zip_exn : ('a, [> read]) t -> ('b, [> read]) t -> ('a * 'b, [< _ perms]) t
  val unzip : ('a * 'b, [> read]) t -> ('a, [< _ perms]) t * ('b, [< _ perms]) t
  val sorted_copy : ('a, [> read]) t -> cmp:('a -> 'a -> int) -> ('a, [< _ perms]) t
  val last : ('a, [> read]) t -> 'a
  val empty : unit -> ('a, [< _ perms]) t
  val equal : ('a, [> read]) t -> ('a, [> read]) t -> equal:('a -> 'a -> bool) -> bool
  val truncate : (_, [> write]) t -> len:int -> unit

  val to_sequence         : ('a, [> read]) t -> 'a Sequence.t
  val to_sequence_mutable : ('a, [> read]) t -> 'a Sequence.t
end
