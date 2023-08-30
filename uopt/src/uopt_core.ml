module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type 'a t = 'a Uopt.t [@@deriving sexp]

    let stable_witness value_witness =
      Stable_witness.of_serializable
        (stable_witness_option value_witness)
        Uopt.of_option
        Uopt.to_option
    ;;

    (* N.B. This [bin_io] implementation is hand-rolled rather than using e.g.
       [Binable.Of_binable1.V2 (Option.V1)] in order to avoid allocating the option. *)
    module Minimal_bin_io = struct
      type nonrec 'a t = 'a t

      let bin_shape_t bin_shape_a =
        Bin_prot.Shape.basetype (Bin_prot.Shape.Uuid.of_string "option") [ bin_shape_a ]
      ;;

      let bin_size_t bin_size_a t =
        match%optional.Uopt t with
        | None -> bin_size_bool false
        | Some a -> bin_size_bool true + bin_size_a a
      ;;

      let bin_write_t bin_write_a buf ~pos t =
        match%optional.Uopt t with
        | None -> bin_write_bool buf ~pos false
        | Some a ->
          let pos = bin_write_bool buf ~pos true in
          bin_write_a buf ~pos a
      ;;

      let bin_read_t bin_read_a buf ~pos_ref =
        match bin_read_bool buf ~pos_ref with
        | false -> Uopt.none
        | true -> Uopt.some (bin_read_a buf ~pos_ref)
      ;;

      let __bin_read_t__
        (_ : _ Bin_prot.Read.reader)
        (_ : Bigstring.V1.t)
        ~pos_ref
        (_ : int)
        =
        Bin_prot.Common.raise_variant_wrong_type "Uopt" !pos_ref
      ;;
    end

    include Bin_prot.Utils.Of_minimal1 (Minimal_bin_io)
  end
end

open! Core
include Stable.V1
include Uopt
