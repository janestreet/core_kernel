type 'a t = 'a array with bin_io, sexp, typerep

include Binary_searchable.S1 with type 'a t := 'a t

(** Note: [Array.length] is not constant for a given array, as one can reduce it with
    [Array.truncate] *)
include Container.S1 with type 'a t := 'a t

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

(** Unsafe version of [get].  Can cause arbitrary behavior when used to for an
    out-of-bounds array access *)
external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"

(** Unsafe version of [set].  Can cause arbitrary behavior when used to for an
    out-of-bounds array access *)
external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"

(** [create ~len x] creates an array of length [len] with the value [x] populated in each
    element *)
val create : len:int -> 'a -> 'a t

(** [init n ~f] creates an array of length [n] where the [i]th element is initialized with
    [f i] (starting at zero) *)
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

(** Same as [Array.append], but concatenates a list of arrays. *)
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
  type nonrec t = int t with bin_io, compare, sexp

  include Blit.S with type t := t

  external unsafe_blit
    : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
    = "core_array_unsafe_int_blit" "noalloc"
end

module Float : sig
  type nonrec t = float t with bin_io, compare, sexp

  include Blit.S with type t := t

  external unsafe_blit
    : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
    = "core_array_unsafe_float_blit" "noalloc"

  (** [alloc len] @return a freshly allocated but uninitialized float array
      of length [len].

      @raise Failure if [len < 0]
  *)
  val alloc : int -> float array

  external unsafe_alloc : int -> float array = "core_array_unsafe_alloc_floats"
end

(** [Array.of_list l] returns a fresh array containing the elements of [l]. *)
val of_list : 'a list -> 'a t

(** [Array.map ~f a] applies function [f] to all the elements of [a],
   and builds an array with the results returned by [f]:
   [[| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |]]. *)
val map : f:('a -> 'b) -> 'a t -> 'b t

(** Same as {!Array.iter}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit

(** Same as {!Array.map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)
val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t

val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

(** [Array.fold_right f a ~init] computes
   [f a.(0) (f a.(1) ( ... (f a.(n-1) init) ...))],
   where [n] is the length of the array [a]. *)
val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

(* All sorting is in increasing order by default. *)

(* [sort] uses constant heap space. [stable_sort] uses linear heap space.

   To sort only part of the array, specify [pos] to be the index to start sorting from
   and [len] indicating how many elements to sort. *)
val sort        : ?pos:int -> ?len:int -> 'a t -> cmp:('a -> 'a -> int) -> unit
val stable_sort : 'a t -> cmp:('a -> 'a -> int) -> unit

val is_sorted : 'a t -> cmp:('a -> 'a -> int) -> bool

(* [is_sorted_strictly xs ~cmp] iff [is_sorted xs ~cmp] and no two consecutive elements
   in [xs] are equal according to [cmp] *)
val is_sorted_strictly : 'a t -> cmp:('a -> 'a -> int) -> bool

(* same as [List.concat_map] *)
val concat_map : 'a t -> f:('a -> 'b array) -> 'b array

val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t

val partitioni_tf : 'a t -> f:(int -> 'a -> bool) -> 'a t * 'a t

val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

(** [normalize array index] returns a new index into the array such that if index is less
    than zero, the returned index will "wrap around" -- i.e. array.(normalize array (-1))
    returns the last element of the array. *)
val normalize : 'a t -> int -> int

(** [slice array start stop] returns a fresh array including elements [array.(start)]
    through [array.(stop-1)] with the small tweak that the start and stop positions are
    normalized and a stop index of 0 means the same thing a stop index of [Array.length
    array].  In summary, it's mostly like the slicing in Python or Matlab.  One difference
    is that a stop value of 0 here is like not specifying a stop value in Python. *)
val slice : 'a t -> int -> int -> 'a t

(** Array access with [normalize]d index. *)
val nget : 'a t -> int -> 'a

(** Array modification with [normalize]d index. *)
val nset : 'a t -> int -> 'a -> unit

(** [filter_opt array] returns a new array where [None] entries are omitted and [Some x]
    entries are replaced with [x]. Note that this changes the index at which elements
    will appear. *)
val filter_opt : 'a option t -> 'a t

(** [filter_map ~f array] maps [f] over [array] and filters [None] out of the results. *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** Same as [filter_map] but uses {!Array.mapi}. *)
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

(* Functions with 2 suffix raise an exception if the lengths aren't the same. *)
val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit

val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val fold2_exn
  :  'a t
  -> 'b t
  -> init:'c
  -> f:('c -> 'a -> 'b -> 'c)
  -> 'c

(** [for_all2_exn t1 t2 ~f] fails if [length t1 <> length t2]. *)
val for_all2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

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
    It raises [Not_found] if there is no such [a].
*)
val find_exn : 'a t -> f:('a -> bool) -> 'a

(** [findi t f] returns the first index [i] of [t] for which [f i t.(i)] is true *)
val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option

(** [findi_exn t f] returns the first index [i] of [t] for which [f i t.(i)] is
    true.  It raises [Not_found] if there is no such element. *)
val findi_exn : 'a t -> f:(int -> 'a -> bool) -> int * 'a

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

(** [combine ar] combines two arrays to an array of pairs. *)
val combine : 'a t -> 'b t -> ('a * 'b) t

(** [split ar] splits an array of pairs into two arrays of single elements. *)
val split : ('a * 'b) t -> 'a t * 'b t

(** [sorted_copy ar cmp] returns a shallow copy of [ar] that is sorted. Similar to
    List.sort *)
val sorted_copy : 'a t -> cmp:('a -> 'a -> int) -> 'a t

val last : 'a t -> 'a

(** [empty ()] creates an empty array *)
val empty : unit -> 'a t

val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool

(** [truncate t ~len] drops [length t - len] elements from the end of [t], changing [t] so
    that [length t = len] afterwards.  [truncate] raises if [len <= 0 || len > length
    t]. *)
val truncate : _ t -> len:int -> unit

module Infix : sig
  val ( <|> ) : 'a t -> int * int -> 'a t
end


(** [to_sequence t] converts [t] to a sequence. [t] is copied internally so that future
    modifications of [t] do not change the sequence. *)
val to_sequence : 'a t -> 'a Sequence.t

(** [to_sequence_mutable t] converts [t] to a sequence. [t] is shared with the sequence
    and modifications of [t] will result in modification of the sequence. *)
val to_sequence_mutable : 'a t -> 'a Sequence.t
