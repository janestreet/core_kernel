module Merge_with_duplicates_element = struct
  type 'a t = 'a Base.Sequence.Merge_with_duplicates_element.t =
    | Left of 'a
    | Right of 'a
    | Both of 'a * 'a
  [@@deriving bin_io]

  include (Base.Sequence.Merge_with_duplicates_element
           : module type of struct include Base.Sequence.Merge_with_duplicates_element end
           with type 'a t := 'a t)
end

include (Base.Sequence
         : module type of struct include Base.Sequence end
         with module Merge_with_duplicates_element := Merge_with_duplicates_element)
