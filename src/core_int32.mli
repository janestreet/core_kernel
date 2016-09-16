include module type of struct include Base.Int32 end
  with module Hex := Base.Int32.Hex

include Int_intf.Extension
  with type t := t
   and type comparator_witness := comparator_witness
