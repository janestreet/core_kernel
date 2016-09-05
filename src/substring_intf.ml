(** A substring is a contiguous set of characters within a string. Creating a substring
    does not copy. Therefore modifying the string also modifies the substring. *)

open! Import

module type S = sig
  (** the type of strings that type t is a substring of *)
  type base

  type t

  include Container.S0 with type t := t with type elt := char

  val base : t -> base

  (** [pos] refers to the position in the base string, not any other substring that this
      substring was generated from. *)
  val pos : t -> int

  (** Per [String.get] and [Bigstring.get], this raises an exception if the index is out
      of bounds. *)
  val get : t -> int -> char

  (** [create ?pos ?len base] creates a substring of the base sequence of
      length [len] starting at position [pos], i.e.

      {[ base.[pos], base.[pos + 1], ... base.[pos + len - 1] ]}

      An exception is raised if any of those indices into [base] are invalid.

      It does not copy the characters, so mutating [base] mutates [t] and vice versa.
  *)
  val create : ?pos:int -> ?len:int -> base -> t

  val sub : ?pos:int -> ?len:int -> t -> t

  (** {6 Blit functions} for copying characters from a substring to and from both strings
      and substrings. *)

  val blit_to_string      : t -> dst:string      -> dst_pos:int            -> unit
  val blit_to_bigstring   : t -> dst:Bigstring.t -> dst_pos:int            -> unit

  val blit_from_string    : t -> src:string      -> src_pos:int -> len:int -> unit
  val blit_from_bigstring : t -> src:Bigstring.t -> src_pos:int -> len:int -> unit

  (** {6 String concatenation} *)

  (** These always copy. *)
  val concat           : t list -> t
  val concat_string    : t list -> string
  val concat_bigstring : t list -> Bigstring.t

  (** {6 Conversion to substrings}.  These may share memory with the thing from which they
      are copying. *)
  val of_bigstring : Bigstring.t -> t
  val of_string    : string      -> t

  (** these two functions perform a copy *)
  val to_bigstring : t -> Bigstring.t
  val to_string    : t -> string

  (** {6 Prefixes and suffixes} The result of these functions share data with their
      input, but don't mutate the underlying string. *)

  val drop_prefix : t -> int -> t
  val drop_suffix : t -> int -> t
  val prefix      : t -> int -> t
  val suffix      : t -> int -> t
end
