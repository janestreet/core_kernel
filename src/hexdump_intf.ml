open! Import

module type S = sig
  type t

  module Hexdump : sig
    type nonrec t = t [@@deriving sexp_of]

    (** [to_string_hum] renders [t] as a multi-line ASCII string in hexdump format.  [pos]
        and [len] select a subrange of [t] to render.  [max_lines] determines the maximum
        number of lines of hex dump to produce.  If the full hex dump exceeds this number,
        lines in the middle are replaced by a single "..."; the beginning and end of the
        hex dump are left intact.  In order to produce at least some readable hex dump, at
        least 4 lines are always produced. *)
    val to_string_hum
      :  ?max_lines : int (** default: [!default_max_lines] *)
      -> ?pos       : int
      -> ?len       : int
      -> t
      -> string

    (** [to_sequence] produces the lines of [to_string_hum] as a sequence of strings.
        This may be useful for incrementally rendering a large hex dump without producing
        the whole thing in memory.  Optional arguments function as in [to_string_hum]. *)
    val to_sequence
      :  ?max_lines : int (** default: [!default_max_lines] *)
      -> ?pos       : int
      -> ?len       : int
      -> t
      -> string Sequence.t
  end
end

module type S1 = sig
  type _ t
  module Hexdump : sig
    type nonrec 'a t = 'a t [@@deriving sexp_of]
    val to_string_hum : ?max_lines:int -> ?pos:int -> ?len:int -> _ t -> string
    val to_sequence   : ?max_lines:int -> ?pos:int -> ?len:int -> _ t -> string Sequence.t
  end
end

module type S2 = sig
  type (_, _) t
  module Hexdump : sig
    type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of]
    val to_string_hum : ?max_lines:int -> ?pos:int -> ?len:int -> (_, _) t -> string
    val to_sequence   : ?max_lines:int -> ?pos:int -> ?len:int -> (_, _) t -> string Sequence.t
  end
end

(** The functor [Hexdump.Of_indexable] uses [length] and [get] to iterate over the
    characters in a value and produce a hex dump. *)
module type Indexable = sig
  type t

  val length : t -> int
  val get    : t -> int -> char
end

module type Indexable1 = sig
  type _ t
  val length : _ t -> int
  val get    : _ t -> int -> char
end

module type Indexable2 = sig
  type (_, _) t
  val length : (_, _) t -> int
  val get    : (_, _) t -> int -> char
end

module type Hexdump = sig
  module type Indexable  = Indexable
  module type Indexable1 = Indexable1
  module type Indexable2 = Indexable2
  module type S          = S
  module type S1         = S1
  module type S2         = S2

  (** Can be used to override the default [~lines] argument for [to_string_hum] and
      [to_sequence] in [S]. *)
  val default_max_lines : int ref

  module Of_indexable  (T : Indexable)  : S  with type          t :=          T.t
  module Of_indexable1 (T : Indexable1) : S1 with type  'a      t :=  'a      T.t
  module Of_indexable2 (T : Indexable2) : S2 with type ('a, 'b) t := ('a, 'b) T.t
end
