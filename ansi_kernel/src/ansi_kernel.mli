(** Common ANSI display attribute definitions.

    NOTE: assorted content lifted from lib/console/src/console.ml *)

module Color : sig
  (** Standard 8 colours. *)
  type t =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    ]
  [@@deriving sexp_of, compare, hash, equal]

  val to_int : [< t ] -> int
end

module Attr : sig
  (** Styling attributes: these provide most of the ANSI display attributes,
      but not directly `Reset, `Blink and `Hidden, so as to explicitly
      discourage their use in general code. *)
  type t =
    [ `Bright
    | `Dim
    | `Underscore
    | `Reverse
    | Color.t
    | `Bg of Color.t
    ]
  [@@deriving sexp_of, compare, hash, equal]

  val to_int : [< t ] -> int
  val list_to_string : [< t ] list -> string
end

module With_all_attrs : sig
  (** All supported (by this library) ANSI display attributes. *)
  type t =
    [ Attr.t
    | `Reset
    | `Blink
    | `Hidden
    ]
  [@@deriving sexp_of, compare, hash, equal]

  val to_int : [< t ] -> int
  val list_to_string : [< t ] list -> string
end
