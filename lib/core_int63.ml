module type S = sig
  include Int_intf.S
  val of_int : int -> t
  val to_int : t -> int option
end

module Arch64 = struct
  include Core_int
  let to_int x = Some x
end

(* select implementation *)
module Arch = 
  (val if Word_size.(word_size = W64) then 
         (module Arch64 : S) 
       else 
         (module Core_int64 : S))

include Arch

