open! Import

include Base.T

module type T_bin = sig type t [@@deriving bin_io] end
