(** This module extends the Base [Option] module with bin_io *)

module Merge_with_duplicates_element : sig
  type 'a t = 'a Base.Sequence.Merge_with_duplicates_element.t =
    | Left of 'a
    | Right of 'a
    | Both of 'a * 'a
  [@@deriving bin_io]

  include module type of struct include Base.Sequence.Merge_with_duplicates_element end
  with type 'a t := 'a t
end

include module type of struct include Base.Sequence end
  with module Merge_with_duplicates_element := Merge_with_duplicates_element
