open! Import

module type S = sig
  type 'a t [@@deriving bin_io]

  include Base.Queue_intf.S with type 'a t := 'a t (** @open *)
end
