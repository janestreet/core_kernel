
type 'a t = 'a option with bin_io, sexp, typerep

include Container.S1 with type 'a t := 'a t

(** Options form a monad, where [return x =  Some x],
    [(None >>= f) = None], and [(Some x >>= f) = f x]. *)
include Monad.S with type 'a t := 'a t

(** [is_none t] returns true iff t = None. *)
val is_none : 'a t -> bool

(** [is_some t] returns true iff t = Some x. *)
val is_some : 'a t -> bool

(** [value_map ~default ~f] is the same as [function Some x -> f x | None -> default] *)
val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

(** [map2 o f] map 'a option and 'b option to a 'c option using ~f *)
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

(** [call x f] run optional function on argument *)
val call : 'a -> f:('a -> unit) t -> unit

(** [apply x f] run optional function on argument and return an option *)
val apply : 'a -> f:('a -> 'b) t -> 'b t

(** [value None ~default] = [default]
    [value (Some x) ~default] = [x]
*)
val value : 'a t -> default:'a -> 'a

(** [value_exn (Some x)] = [x].  [value_exn None] raises an error whose contents contain
    the supplied [~here], [~error], and [message], or a default message if none are
    supplied. *)
val value_exn
  :  ?here:Source_code_position0.t
  -> ?error:Error.t
  -> ?message:string
  -> 'a t
  -> 'a

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val some : 'a -> 'a t

val both : 'a t -> 'b t -> ('a * 'b) t

val first_some : 'a t -> 'a t -> 'a t

val some_if : bool -> 'a -> 'a t

(** [merge a b ~f] merges together the values from [a] and [b] using [f].  If both [a] and
    [b] are [None], returns [None].  If only one is [Some], returns that one, and if both
    are [Some], returns [Some] of the result of applying [f] to the contents of [a] and
    [b]. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t

val filter : f:('a -> bool) -> 'a t -> 'a t

(** [try_with f] returns [Some x] if [f] returns [x] and [None] if [f] raises an
    exception.  See [Result.try_with] if you'd like to know which exception. *)
val try_with : (unit -> 'a) -> 'a t

(** Compares [None] as smaller than any [Some x] *)
val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int

val validate : none:unit Validate.check -> some:'a Validate.check -> 'a t Validate.check
