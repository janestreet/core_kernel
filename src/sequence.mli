(** This module extends the Base [Option] module with bin_io *)

module Merge_with_duplicates_element : sig
  type ('a, 'b) t = ('a, 'b) Base.Sequence.Merge_with_duplicates_element.t =
    | Left of 'a
    | Right of 'b
    | Both of 'a * 'b
  [@@deriving bin_io]

  include module type of struct include Base.Sequence.Merge_with_duplicates_element end
  with type ('a, 'b) t := ('a, 'b) t
end

include module type of struct include Base.Sequence end
  with module Merge_with_duplicates_element := Merge_with_duplicates_element
