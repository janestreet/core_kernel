include Bin_prot.Binable
open Sexplib.Std
open Bin_prot.Std

module List = ListLabels

module Stable = struct

  module Of_binable = struct
    module V1
        (Binable : S)
        (M : sig
           type t
           val to_binable : t -> Binable.t
           val of_binable : Binable.t -> t
         end)
      : S with type t := M.t
      = Bin_prot.Utils.Make_binable (struct
        module Binable = Binable
        include M
      end)
  end

  module Of_binable1 = struct
    module V1
        (Binable : S1)
        (M : sig
           type 'a t
           val to_binable : 'a t -> 'a Binable.t
           val of_binable : 'a Binable.t -> 'a t
         end)
      : S1 with type 'a t := 'a M.t
      = Bin_prot.Utils.Make_binable1 (struct
        module Binable = Binable
        include M
      end)
  end

  module Of_binable2 = struct
    module V1
        (Binable : S2)
        (M : sig
           type ('a, 'b) t
           val to_binable : ('a, 'b) t -> ('a, 'b) Binable.t
           val of_binable : ('a, 'b) Binable.t -> ('a, 'b) t
         end)
      : S2 with type ('a, 'b) t := ('a, 'b) M.t
      = Bin_prot.Utils.Make_binable2 (struct
        module Binable = Binable
        include M
      end)
  end

  module Of_sexpable  = struct
    module V1 (M : Sexpable.S) =
      Of_binable.V1
        (struct
          module Sexp = Sexplib.Sexp
          type t = Sexp.t = Atom of string | List of t list [@@deriving bin_io]
        end)
        (struct
          type t = M.t
          let to_binable = M.sexp_of_t
          let of_binable = M.t_of_sexp
        end)
  end

  module Of_stringable = struct
    module V1 (M : Stringable.S) =
      Bin_prot.Utils.Make_binable (struct
        module Binable = struct
          type t = string [@@deriving bin_io]
        end
        type t = M.t
        let to_binable = M.to_string

        (* Wrap exception for improved diagnostics. *)
        exception Of_binable of string * exn [@@deriving sexp]
        let of_binable s =
          try
            M.of_string s
          with x ->
            raise (Of_binable (s, x))
      end)
  end

end

open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t


type 'a m = (module S with type t = 'a)

let of_bigstring (type a) m bigstring =
  let module M = (val m : S with type t = a) in
  let pos_ref = ref 0 in
  let t = M.bin_read_t bigstring ~pos_ref in
  assert (!pos_ref = Array1.dim bigstring);
  t
;;

(* Using the [Bigstring] module would introduce a cyclic dependency. *)
let create_bigstring size = Array1.create Bigarray.char Bigarray.c_layout size

let to_bigstring ?(prefix_with_length = false) (type a) m t =
  let module M = (val m : S with type t = a) in
  let t_length = M.bin_size_t t in
  let bigstring_length =
    if prefix_with_length then
      t_length + 8 (* the size of a 64-bit int *)
    else
      t_length
  in
  let bigstring = create_bigstring bigstring_length in
  let pos =
    if prefix_with_length then
      Bin_prot.Write.bin_write_int_64bit bigstring ~pos:0 t_length
    else
      0
  in
  let pos = M.bin_write_t bigstring ~pos t in
  assert (pos = bigstring_length);
  bigstring
;;

module Of_binable    = Stable.Of_binable.V1
module Of_binable1   = Stable.Of_binable1.V1
module Of_binable2   = Stable.Of_binable2.V1
module Of_sexpable   = Stable.Of_sexpable.V1
module Of_stringable = Stable.Of_stringable.V1
