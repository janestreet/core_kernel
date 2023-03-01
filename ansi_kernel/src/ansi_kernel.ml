module Stable = struct
  open! Core.Core_stable

  module Color_256 = struct
    module V1 = Color_256.Stable.V1
  end

  module Color = struct
    module V1 = struct
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
      [@@deriving sexp, compare, hash, equal]

      type t =
        [ primary
        | `Color_256 of Color_256.V1.t
        ]
      [@@deriving sexp, compare, hash, equal]
    end

    module V2 = struct
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
      [@@deriving sexp, compare, hash, equal]

      type t =
        [ primary
        | `Color_256 of Color_256.V1.t
        | `Default_color
        ]
      [@@deriving sexp, compare, hash, equal]

      let of_v1 (t : V1.t) = (t :> t)

      let to_v1 (t : t) ~foreground =
        match t with
        | #V1.t as t -> t
        | `Default_color ->
          (* Preserve old behaviour of assuming terminal is white-on-black *)
          if foreground then `White else `Black
      ;;

      let primary_of_v1 (t : V1.primary) : primary = t
      let primary_to_v1 (t : primary) : V1.primary = t
    end
  end

  module Attr = struct
    module V1 = struct
      type t =
        [ `Bright
        | `Dim
        | `Underscore
        | `Reverse
        | Color.V1.t
        | `Bg of Color.V1.t
        ]
      [@@deriving sexp, compare, hash, equal]
    end

    module V2 = struct
      type t =
        [ `Bright
        | `Dim
        | `Underscore
        | `Reverse
        | Color.V2.t
        | `Bg of Color.V2.t
        ]
      [@@deriving sexp, compare, hash, equal]

      let of_v1 (t : V1.t) = (t :> t)

      let to_v1 (t : t) : V1.t =
        match t with
        | #Color.V2.t as fg -> Color.V2.to_v1 fg ~foreground:true
        | `Bg bg -> `Bg (Color.V2.to_v1 bg ~foreground:false)
        | (`Bright | `Dim | `Underscore | `Reverse) as t -> t
      ;;
    end
  end
end

open! Core
module Color_256 = Color_256

(* NOTE: assorted content lifted from lib/console/src/console.ml *)
module Color = struct
  type primary = Stable.Color.V2.primary [@@deriving sexp_of, compare, hash, equal]
  type t = Stable.Color.V2.t [@@deriving sexp_of, compare, hash, equal]

  let to_int_list = function
    | `Black -> [ 30 ]
    | `Red -> [ 31 ]
    | `Green -> [ 32 ]
    | `Yellow -> [ 33 ]
    | `Blue -> [ 34 ]
    | `Magenta -> [ 35 ]
    | `Cyan -> [ 36 ]
    | `White -> [ 37 ]
    | `Color_256 c -> [ 38; 5; Color_256.to_int c ]
    | `Default_color -> [ 39 ]
  ;;
end

module Attr = struct
  type t = Stable.Attr.V2.t [@@deriving sexp_of, compare, hash, equal]

  let to_int_list = function
    | `Bright -> [ 1 ]
    | `Dim -> [ 2 ]
    | `Underscore -> [ 4 ]
    | `Reverse -> [ 7 ]
    (* Background colors are 40..47, foreground 30..37.
       256-color codes start with 48 (bg) or 38 (fg). *)
    | #Color.t as c -> Color.to_int_list c
    | `Bg bg ->
      (match Color.to_int_list bg with
       | ansi_code :: rest -> (ansi_code + 10) :: rest
       | [] -> [] (* NOTE: impossible, but appropriate *))
  ;;

  let list_to_string = function
    | [] -> ""
    | l ->
      sprintf
        "\027[%sm"
        (String.concat
           ~sep:";"
           (List.concat_map l ~f:(fun att -> to_int_list att |> List.map ~f:string_of_int)))
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

  let to_int_list = function
    | `Reset -> [ 0 ]
    | `Blink -> [ 5 ]
    | `Hidden -> [ 8 ]
    | #Attr.t as attr -> Attr.to_int_list attr
  ;;

  let list_to_string = function
    | [] -> ""
    | l ->
      sprintf
        "\027[%sm"
        (String.concat
           ~sep:";"
           (List.concat_map l ~f:(fun att -> to_int_list att |> List.map ~f:string_of_int)))
  ;;
end
