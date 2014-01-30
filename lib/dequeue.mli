(** A double ended queue that can shrink and expand on both ends.

    An index is assigned to an element when it enters the queue, and the index of an
    element is static (i.e. an index refers to a distinct element until that element is
    removed from the queue, no matter how many intervening push/pop operations occur).

    One consequence of this is that the minimum index may be < 0.

    The "front" is the smallest valid index, while the "back" is the largest.

    All operations are amortized O(1) with a small constant. *)

type 'a t with bin_io, sexp

include Binary_searchable.S1 with type 'a t := 'a t
include Container.        S1 with type 'a t := 'a t

(** [create ?initial_length ?never_shrink ()] create a new [t].  [initial_length] is the
    initial length of the dequeue; it will be able to hold [initial_length] elements
    without resizing.  It must be positive.  If [never_shrink] is true, the physical array
    will never shrink; only expand.  If [initial_length] is given without [never_shrink]
    then [never_shrink] is presumed to be [true], otherwise [never_shrink] defaults to
    [false]. *)
val create
  :  ?initial_length:int  (* defaults to 7 *)
  -> ?never_shrink:bool   (* see comment above *)
  -> unit
  -> _ t

(** [front_index t] return the index of the front item in [t]. *)
val front_index : _ t -> int option

(** [front_index_exn t] throws an exception if [t] is empty, otherwise returns the index
    of the front item in [t] *)
val front_index_exn : _ t -> int

(** [back_index t] return the index of the back item in [t]. *)
val back_index : _ t -> int option

(** [back_index_exn t] throws an exception if [t] is empty, otherwise returns the index
    of the back item in [t] *)
val back_index_exn : _ t -> int

(** [get_opt t i] return the element at index [i].  Return [None] if [i] is invalid. *)
val get_opt : 'a t -> int -> 'a option

(** [get t i] return the element at index [i].  Raise an exception if [i] is
    invalid. *)
val get : 'a t -> int -> 'a

(** [peek t back_or_front] return the value at the back or front of the dequeue without
    removing it. *)
val peek           : 'a t -> [ `back | `front ] -> 'a option
val peek_front     : 'a t -> 'a option
val peek_front_exn : 'a t -> 'a
val peek_back      : 'a t -> 'a option
val peek_back_exn  : 'a t -> 'a

(** [set_exn t i v] mutate the element at [i]. *)
val set_exn : 'a t -> int -> 'a -> unit

(** [iter' t ~f] iter over the elements of [t]. *)
val iter' : 'a t -> [ `front_to_back | `back_to_front ] -> f:('a -> unit) -> unit

(** [iteri t ~f] iter over the elements of t [`front_to_back] passing in the index. *)
val iteri : 'a t -> f : (int -> 'a -> unit) -> unit

(** [iteri' t ~f] as [iter], but also passes in the index of the current element. *)
val iteri' : 'a t -> [ `front_to_back | `back_to_front ] -> f:(int -> 'a -> unit) -> unit

(** [fold' t ~init ~f] fold over the elements of [t] *)
val fold'
  :  'a t
  -> [ `front_to_back | `back_to_front ]
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b

(** [foldi t ~init ~f] as [fold], but also passes in the index of the current element. *)
val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b

(** [foldi' t ~init ~f] as [fold'], but also passes in the index of the current element to
    [f]. *)
val foldi'
  :  'a t
  -> [ `front_to_back | `back_to_front ]
  -> init:'b
  -> f:(int -> 'b -> 'a -> 'b)
  -> 'b

(** [enqueue t back_or_front v] push [v] onto the [back_or_front] of [t]. *)
val enqueue       : 'a t -> [ `back | `front ] -> 'a -> unit
val enqueue_front : 'a t ->                       'a -> unit
val enqueue_back  : 'a t ->                       'a -> unit

(** [clear t] removes all elements from [t]. *)
val clear : _ t -> unit

(** [drop ?n t back_or_front] drop [n] elements (default 1) from the [back_or_front] of
    [t].  If [t] has fewer than [n] elements then it is cleared. *)
val drop       : ?n:int -> _ t -> [ `back | `front ] -> unit
val drop_front : ?n:int -> _ t -> unit
val drop_back  : ?n:int -> _ t -> unit

(** [dequeue t back_or_front] remove and return the [back_or_front] of [t] *)
val dequeue           : 'a t -> [ `back | `front ] -> 'a option
val dequeue_exn       : 'a t -> [ `back | `front ] -> 'a
val dequeue_front     : 'a t -> 'a option
val dequeue_front_exn : 'a t -> 'a
val dequeue_back      : 'a t -> 'a option
val dequeue_back_exn  : 'a t -> 'a
