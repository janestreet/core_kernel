open! Core_kernel

(* NOTE: assorted content lifted from lib/console/src/console.ml *)
module Color = struct
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

  let to_int = function
    | `Black -> 30
    | `Red -> 31
    | `Green -> 32
    | `Yellow -> 33
    | `Blue -> 34
    | `Magenta -> 35
    | `Cyan -> 36
    | `White -> 37
  ;;
end

module Attr = struct
  type t =
    [ `Bright
    | `Dim
    | `Underscore
    | `Reverse
    | Color.t
    | `Bg of Color.t
    ]
  [@@deriving sexp_of, compare, hash, equal]

  let to_int = function
    | `Bright -> 1
    | `Dim -> 2
    | `Underscore -> 4
    | `Reverse -> 7
    (* Background colors are 40..47, foreground 30..37. *)
    | #Color.t as c -> Color.to_int c
    | `Bg bg -> Color.to_int bg + 10
  ;;

  let list_to_string = function
    | [] -> ""
    | l ->
      sprintf
        "\027[%sm"
        (String.concat ~sep:";" (List.map l ~f:(fun att -> string_of_int (to_int att))))
  ;;
end

module With_all_attrs = struct
  type t =
    [ Attr.t
    | `Reset
    | `Blink
    | `Hidden
    ]
  [@@deriving sexp_of, compare, hash, equal]

  let to_int = function
    | `Reset -> 0
    | `Blink -> 5
    | `Hidden -> 8
    | #Attr.t as attr -> Attr.to_int attr
  ;;

  let list_to_string = function
    | [] -> ""
    | l ->
      sprintf
        "\027[%sm"
        (String.concat ~sep:";" (List.map l ~f:(fun att -> string_of_int (to_int att))))
  ;;
end
