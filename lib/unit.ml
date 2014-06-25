open Typerep_lib.Std
open Sexplib.Std
open Bin_prot.Std

module T = struct
  type t = unit with sexp, bin_io, typerep

  let compare _ _ = 0
  let hash _ = 0
end

include T
include Sexpable.To_stringable (T)
include Comparable.Make_binable (T)
include Hashable.Make_binable (T)

include Pretty_printer.Register (struct
  type nonrec t = t
  let to_string = to_string
  let module_name = "Core.Std.Unit"
end)

module type S = sig end

type m = (module S)
