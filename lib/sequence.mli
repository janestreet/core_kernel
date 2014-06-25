(** A sequence of elements that can be produced one at a time, on demand, normally with no
    sharing.

    The elements are computed on demand, possibly repeating work if they are demanded
    multiple times.  A sequence can be built by unfolding from some initial state, which
    will in practice often be other containers.

    Most functions constructing a sequence will not immediately compute any elements of
    the sequence.  These functions will always return in O(1), but traversing the
    resulting sequence may be more expensive.  The most they will do immediately is
    generate a new internal state and a new step function.

    Functions that transform existing sequences sometimes have to reconstruct some suffix
    of the input sequence, even if it is unmodified.  For example, calling [drop 1] will
    return a sequence with a slightly larger state and whose elements all cost slightly
    more to traverse.  Because this is sometimes undesirable (for example, applying [drop
    1] n times will cost O(n) per element traversed in the result), there are also more
    eager versions of many functions (whose names are suffixed with [_eagerly]) that do
    more work up front.  A function has the [_eagerly] suffix iff it matches both of these
    conditions:

      * It might consume an element from an input [t] before returning.

      * It only returns a [t] (not paired with something else, not wrapped in an [option],
        etc.).  If it returns anything other than a [t] and it has at least one [t] input,
        it's probably demanding elements from the input [t] anyway.

    Only [*_exn] functions can raise exceptions, except if the function underlying the
    sequence (the [f] passed to [unfold]) raises, in which case the exception will
    cascade. *)

type +'a t
type 'a sequence = 'a t

include Container.S1 with type 'a t := 'a t
include Monad.S      with type 'a t := 'a t

(** [empty] is a sequence with no elements. *)
val empty : _ t

(** [next] returns the next element of a sequence and the next tail if the sequence is not
    finished. It is the most primitive way to walk over a sequence. *)
val next : 'a t -> ('a * 'a t) option


(** A [Step] describes the next step of the sequence construction.  [Done] indicates the
    sequence is finished.  [Skip] indicates the sequence continues with another state
    without producing the next element yet.  [Yield] outputs an element and introduces a
    new state. *)
module Step : sig
  type ('a, 's) t =
    | Done
    | Skip of 's
    | Yield of 'a * 's
end

(** [unfold_step ~init ~f] constructs a sequence by giving an initial state [init] and a
    function [f] explaining how to continue the next step from a given state. *)
val unfold_step : init:'s -> f:('s -> ('a, 's) Step.t) -> 'a t

(** [unfold ~init f] is a simplified version of [unfold_step] that does not allow
    [Skip]. *)
val unfold : init:'s -> f:('s -> ('a * 's) option) -> 'a t

(** [unfold_with t ~init ~f] folds a state through the sequence [t] to create a new
    sequence *)
val unfold_with : 'a t -> init:'s -> f:('s -> 'a -> ('b, 's) Step.t) -> 'b t

(** return the nth element *)
val nth     : 'a t -> int -> 'a option
val nth_exn : 'a t -> int -> 'a

val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

val filteri : 'a t -> f: (int -> 'a -> bool) -> 'a t

(** [merge t1 t2 ~cmp] produces the interleaved elements of [t1] and [t2], always picking
    the smallest of the two available elements from [t1] and [t2], according to [cmp].
    When the two available elements are equal, the one from [t1] is preferred. *)
val merge : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> 'a t

val hd     : 'a t -> 'a option
val hd_exn : 'a t -> 'a

(** [tl t] and [tl_eagerly_exn t] immediately evaluate the first element of [t] and return
    the unevaluated tail. *)
val tl             : 'a t -> 'a t option
val tl_eagerly_exn : 'a t -> 'a t

val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option

(** [find_exn t ~f] returns the first element of [t] that satisfies [f]. It raises if
    there is no such element. *)
val find_exn : 'a t -> f:('a -> bool) -> 'a

(** [append t1 t2] first produces the elements of [t1], then produces the elements of
    [t2]. *)
val append : 'a t -> 'a t -> 'a t

(** [concat tt] produces the elements of each inner sequence sequentially. *)
val concat : 'a t t -> 'a t

(** [concat_map t ~f] is [concat (map t ~f)].*)
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t

(** [concat_mapi t ~f] is like concat_map, but passes the index as an argument. *)
val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) -> 'b t

(** Transforms a pair of sequences into a sequence of pairs. The length of the returned
    sequence is the length of the shorter input. The remaining elements of the longer
    input are discarded.

    WARNING: Unlike [List.zip], this will not error out if the two input sequences are of
    different lengths, because [zip] may have already returned some elements by the time
    this becomes apparent. *)
val zip : 'a t -> 'b t -> ('a * 'b) t

(** [zip_full] is like [zip], but if one sequence ends before the other, then it keeps
    producing elements from the other sequence until it has ended as well. *)
val zip_full: 'a t -> 'b t -> [ `Left of 'a | `Both of 'a * 'b | `Right of 'b ] t

(** [iteri] is just like [iter], but it also passes in the index of each element to
    [f]. *)
val iteri : 'a t ->  f:(int -> 'a -> unit) -> unit

(** [foldi] is just like [fold], but it also passes in the index of each element to
    [f]. *)
val foldi : 'a t -> f:(int -> 'b -> 'a -> 'b) -> init:'b -> 'b

(** [reduce_exn f [a1; ...; an]] is [f (... (f (f a1 a2) a3) ...) an]. It fails on the
    empty sequence. *)
val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option

(** [find_consecutive_duplicate t ~equal] returns the first pair of consecutive elements
    [(a1, a2)] in [t] such that [equal a1 a2].  They are returned in the same order as
    they appear in [t]. *)
val find_consecutive_duplicate : 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option

(** The same sequence with consecutive duplicates removed.  The relative order of the
    other elements is unaffected. *)
val remove_consecutive_duplicates : 'a t -> equal:('a -> 'a -> bool) -> 'a t

(** [range ?stride ?start ?stop start_i stop_i] is the sequence of integers from [start_i]
    to [stop_i], stepping by [stride].  If [stride] < 0 then we need [start_i] > [stop_i]
    for the result to be nonempty (or [start_i] >= [stop_i] in the case where both bounds
    are inclusive). *)
val range
  :  ?stride:int                            (** default is [1] *)
  -> ?start:[`inclusive|`exclusive]         (** default is [`inclusive] *)
  -> ?stop:[`inclusive|`exclusive]          (** default is [`exclusive] *)
  -> int
  -> int
  -> int t

(** [init n ~f] is [[(f 0); (f 1); ...; (f (n-1))]].  It is an error if [n < 0]. *)
val init : int -> f:(int -> 'a) -> 'a t

(** [filter_mapi] is just like [filter_map], but it also passes in the index of each
    element to [f]. *)
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

(** [filter_opt t] produces the elements of [t] which are not [None].  [filter_opt t] =
    [filter_map t ~f:ident] *)
val filter_opt : 'a option t -> 'a t

(** [sub t ~pos ~len] is the [len]-element subsequence of [t], starting at [pos].  If the
    sequence is shorter than [pos + len], it returns [ t[pos] ... t[l-1] ], where [l] is
    the length of the sequence. *)
val sub : 'a t -> pos:int -> len:int -> 'a t

(** [take t n] produces the first [n] elements of [t]. *)
val take : 'a t -> int -> 'a t

(** [drop t n] produces all elements of [t] except the first [n] elements.  If there are
    fewer than [n] elements in [t], there is no error; the resulting sequence simply
    produces no elements.  Usually you will probably want to use [drop_eagerly] because it
    can be significantly cheaper. *)
val drop : 'a t -> int -> 'a t

(** [drop_eagerly t n] immediately consumes the first [n] elements of [t] and returns the
    unevaluated tail of [t]. *)
val drop_eagerly : 'a t -> int -> 'a t

(** [take_while t ~f] produces the longest prefix of [t] for which [f] applied to each
    element is [true]. *)
val take_while : 'a t -> f : ('a -> bool) -> 'a t

(** [drop_while t ~f] produces the suffix of [t] beginning with the first element of [t]
    for which [f] is [false].  Usually you will probably want to use [drop_while_option]
    because it can be significantly cheaper. *)
val drop_while : 'a t -> f : ('a -> bool) -> 'a t

(** [drop_while_option t ~f] immediately consumes the elements from [t] until the
    predicate [f] fails and returns the first element that failed along with the
    unevaluated tail of [t].  The first element is returned separately because the
    alternatives would mean forcing the consumer to evaluate the first element again (if
    the previous state of the sequence is returned) or take on extra cost for each element
    (if the element is added to the final state of the sequence using [shift_right]). *)
val drop_while_option : 'a t -> f : ('a -> bool) -> ('a * 'a t) option

(** [split_n_eagerly t n] immediately consumes the first [n] elements of [t] and returns
    the consumed prefix, as a new stream, along with the unevaluated tail of [t]. *)
val split_n_eagerly : 'a t -> int -> 'a t * 'a t

(** [shift_right t a] produces [a] and then produces each element of [t]. *)
val shift_right : 'a t -> 'a -> 'a t

(** [shift_right_with_list t l] produces the elements of [l], then produces the elements
    of [t].  It is better to call [shift_right_with_list] with a list of size n than
    [shift_right] n times; the former will require O(1) work per element produced and the
    later O(n) work per element produced. *)
val shift_right_with_list : 'a t -> 'a list -> 'a t

(** [shift_left t n] is a synonym for [drop t n].*)
val shift_left : 'a t -> int -> 'a t

module Infix : sig
  val ( @ ) : 'a t -> 'a t -> 'a t
end

(** Returns a sequence with all possible pairs.  The stepper function of the second
    sequence passed as argument may be applied to the same state multiple times, so be
    careful using [cartesian_product] with expensive or side-effecting functions. *)
val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

(** [intersperse xs ~sep] produces [sep] between adjacent elements of [xs].
    e.g. [intersperse [1;2;3] ~sep:0 = [1;0;2;0;3]] *)
val intersperse : 'a t -> sep:'a -> 'a t

(** [cycle t] repeats the sequence [t] forever.  The elements of [t] will be recomputed
    for each repetition in the cycle. *)
val cycle : 'a t -> 'a t

(** [repeat a] repeats [a] forever. *)
val repeat : 'a -> ' a t

(** [singleton a] produces [a] exactly once. *)
val singleton : 'a -> 'a t

(** [delayed_fold] allows to do an on-demand fold, while maintaning a state.  This
    function is sufficient to implement [fold_m] in any monad.

    {[
      let fold_m t ~init ~f =
        let open M in
        delayed_fold t ~init
          ~f:(fun s a ~k -> f s a >>= k)
          ~finish:return
    ]}

    It is possible to exit early by not calling [k] in [f]. It is also possible to call
    [k] multiple times. This results in the rest of the sequence being folded over
    multiple times, independently.
*)
val delayed_fold
  :  'a t
  -> init:'s
  -> f:('s -> 'a -> k:('s -> 'r) -> 'r) (* [k] stand for "continuation" *)
  -> finish:('s -> 'r)
  -> 'r

val to_list : 'a t -> 'a list

(** [to_list_rev t] returns a list of the elements of [t], in reverse order. It is faster
    than [to_list]. *)
val to_list_rev : 'a t -> 'a list

val of_list : 'a list -> 'a t

(** [memoize t] produces each element of [t], but also memoizes them so that if you
    consume the same element multiple times it is only computed once.  It's a non-eager
    version of [force_eagerly]. *)
val memoize : 'a t -> 'a t

(** [force_eagerly t] precomputes the sequence.  It is behaviorally equivalent to [of_list
    (to_list t)], but may at some point have a more efficient implementation.  It's an
    eager version of [memoize]. *)
val force_eagerly : 'a t -> 'a t

(** [bounded_length ~at_most t] returns [`Is len] if [len = length t <= at_most], and
    otherwise returns [`Greater].  Walks through only as much of the sequence as
    necessary.  Always returns [`Greater] if [at_most < 0]. *)
val bounded_length : _ t -> at_most:int -> [ `Is of int | `Greater ]

(** [length_is_bounded_by ~min ~max t] returns true if [min <= length t] and [length t <=
    max] When [min] or [max] are not provided, the check for that bound is omitted.  Walks
    through only as much of the sequence as necessary. *)
val length_is_bounded_by: ?min:int -> ?max:int -> _ t -> bool

(** [Generator] is a monadic interface to generate sequences in a direct style, similar to
    Python's generators.

    Here are some examples:

    {[
      open Generator

      let rec traverse_list = function
        | [] -> return ()
        | x :: xs -> yield x >>= fun () -> traverse_list xs

      let traverse_option = function
        | None -> return ()
        | Some x -> yield x

      let traverse_array arr =
        let n = Array.length arr in
        let rec loop i =
          if i >= n then return () else yield arr.(i) >>= fun () -> loop (i + 1)
        in
        loop 0

      let rec traverse_bst = function
        | Node.Empty -> return ()
        | Node.Branch (left, value, right) ->
          traverse_bst left  >>= fun () ->
          yield        value >>= fun () ->
          traverse_bst right

      let sequence_of_list   x = Generator.run (traverse_list   x)
      let sequence_of_option x = Generator.run (traverse_option x)
      let sequence_of_array  x = Generator.run (traverse_array  x)
      let sequence_of_bst    x = Generator.run (traverse_bst    x)
    ]}
*)

module Generator : sig
  include Monad.S2
  val yield : 'elt -> (unit, 'elt) t
  val run : (unit, 'elt) t -> 'elt sequence
end
