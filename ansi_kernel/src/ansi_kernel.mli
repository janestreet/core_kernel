(** Common ANSI display attribute definitions.

    NOTE: assorted content lifted from lib/console/src/console.ml *)

module Color_256 = Color_256

module Color : sig
  type primary =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    ]

  (** Standard 8 colors and 256-color palette. The [`Default_color]s depend on the
      terminal but are likely to be the same as [`White] and [`Black] (and which one is
      foreground vs background will depend on whether the terminal is white-on-black or
      black-on-white) *)
  type t =
    [ primary
    | `Color_256 of Color_256.t
    | `Default_color
    ]
  [@@deriving sexp_of, compare, hash, equal]

  val to_int_list : [< t ] -> int list
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

  val to_int_list : [< t ] -> int list
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

  val to_int_list : [< t ] -> int list
  val list_to_string : [< t ] list -> string
end

module Stable : sig
  module Color : sig
    module V1 : sig
      type primary
      type t [@@deriving sexp, compare, hash, equal]
    end

    module V2 : sig
      type primary = Color.primary
      type t = Color.t [@@deriving sexp, compare, hash, equal]

      val of_v1 : V1.t -> t
      val to_v1 : t -> foreground:bool -> V1.t
      val primary_of_v1 : V1.primary -> primary
      val primary_to_v1 : primary -> V1.primary
    end
  end

  module Attr : sig
    module V1 : sig
      type t [@@deriving sexp, compare, hash, equal]
    end

    module V2 : sig
      type t = Attr.t [@@deriving sexp, compare, hash, equal]

      val of_v1 : V1.t -> t
      val to_v1 : t -> V1.t
    end
  end
end
