module Core_source_code_position0 = Source_code_position0
open! Import

module Int = Core_int
module String = Core_string

module T = struct
  include Core_source_code_position0

  let hash { Lexing. pos_fname; pos_lnum; pos_bol; pos_cnum } =
    String.hash pos_fname
    lxor Int.hash pos_lnum
    lxor Int.hash pos_bol
    lxor Int.hash pos_cnum
  ;;
end

include T
include Comparable.Make_using_comparator (T)
include Hashable  .Make (T)
