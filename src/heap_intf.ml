module type S = sig
  (** of_sexp and bin_io functions aren't supplied for heaps due to the difficulties in
      reconstructing the correct comparison function when de-serializing. *)
  type 'a t [@@deriving sexp_of]

  (** Mutation of the heap during iteration is not supported, but there is no check to
      prevent it.  The behavior of a heap that is mutated during iteration is
      undefined. *)
  include Container.S1 with type 'a t := 'a t

  (** Even though these two functions are part of Container.S1, they are documented
      separately to make sure there is no confusion.  They are independent of the
      comparison function used to order the heap.  Instead, a traversal of the entire
      structure is done using the provided [cmp] function to find a min or max.

      If you want to access the smallest element of the heap according to the heap's
      comparison function in constant time, you should use [top]. *)
  val min_elt : 'a t -> cmp:('a -> 'a -> int) -> 'a option
  val max_elt : 'a t -> cmp:('a -> 'a -> int) -> 'a option

  (** [create ?min_size ~cmp] returns a new min-heap that can store [min_size] elements
      without reallocations, using ordering function [cmp].

      The top of the heap is the smallest element as determined by the provided comparison
      function.  In particular, if [cmp x y < 0] then [x] will be "on top of" [y] in the
      heap.

      Memory use is surprising in two ways:

      1. The underlying pool never shrinks, so current memory use will at least be
      proportional to the largest number of elements that the heap has ever held.

      2. Not all the memory is freed upon [remove], but rather after some number of
      subsequent [pop] operations. Alternating [add] and [remove] operations can therefore
      use unbounded memory.
  *)
  val create : ?min_size : int -> cmp:('a -> 'a -> int) -> unit -> 'a t

  (** [min_size] (see [create]) will be set to the size of the input array or list. *)
  val of_array : 'a array -> cmp:('a -> 'a -> int) -> 'a t
  val of_list  : 'a list  -> cmp:('a -> 'a -> int) -> 'a t

  (** returns the top (i.e., smallest) element of the heap *)
  val top     : 'a t -> 'a option
  val top_exn : 'a t -> 'a

  val add : 'a t -> 'a -> unit

  (** [remove_top t] does nothing if [t] is empty *)
  val remove_top : _ t -> unit

  (** This removes and returns the top (i.e. least) element *)
  val pop     : 'a t -> 'a option
  val pop_exn : 'a t -> 'a

  (** [pop_if t cond] returns [Some top_element] of [t] if it satisfies condition
      [cond], removing it, or [None] in any other case. *)
  val pop_if : 'a t -> ('a -> bool) -> 'a option

  (** [copy t] returns a shallow copy *)
  val copy : 'a t -> 'a t
end
