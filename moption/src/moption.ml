module Exposed_for_use_in_stable = struct
  open! Core
  open! Import

  (* Being a pointer, no one outside this module can construct a value that is
     [phys_same] as this one.

     this code is duplicated in Option_array.Cheap_option, and if we find yet another
     place where we want it we should reconsider making it shared. *)
  let none = Obj.obj (Obj.new_block Obj.abstract_tag 1)
  let create () = ref none
  let is_none x = phys_equal !x none
  let get t = if is_none t then None else Some !t
  let unsafe_get t = !t

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unsafe_get
    end
  end
end

module Stable = struct
  open! Core.Core_stable
  open Exposed_for_use_in_stable

  module V1 = struct
    type 'a t = 'a ref [@@deriving stable_witness]

    include
      Sexpable.Of_sexpable1.V1
        (Option.V1)
        (struct
          type nonrec 'a t = 'a t

          let to_sexpable = get

          let of_sexpable = function
            | None -> create ()
            | Some v -> ref v
          ;;
        end)

    (* N.b. this [bin_io] implementation is hand-rolled rather than using e.g.
       [Binable.Of_binable1.V2 (Option.V1)] in order to avoid allocating the option. *)
    module Minimal_bin_io = struct
      type nonrec 'a t = 'a t

      let bin_shape_t bin_shape_a =
        Bin_prot.Shape.basetype
          (Bin_prot.Shape.Uuid.of_string "afef8a9c-daba-11ed-a4e5-aa777790ac98")
          [ bin_shape_a ]
      ;;

      let bin_size_t bin_size_a t =
        match%optional (t : _ t) with
        | None -> bin_size_bool false
        | Some a -> bin_size_bool true + bin_size_a a
      ;;

      let bin_write_t bin_write_a buf ~pos t =
        match%optional (t : _ t) with
        | None -> bin_write_bool buf ~pos false
        | Some a ->
          let pos = bin_write_bool buf ~pos true in
          bin_write_a buf ~pos a
      ;;

      let bin_read_t bin_read_a buf ~pos_ref =
        match bin_read_bool buf ~pos_ref with
        | false -> create ()
        | true -> ref (bin_read_a buf ~pos_ref)
      ;;

      let __bin_read_t__
        (_ : _ Bin_prot.Read.reader)
        (_ : Bigstring.V1.t)
        ~pos_ref
        (_ : int)
        =
        Bin_prot.Common.raise_variant_wrong_type "Moption" !pos_ref
      ;;
    end

    include Bin_prot.Utils.Of_minimal1 (Minimal_bin_io)
  end
end

open! Core
open! Import
include Stable.V1
include Exposed_for_use_in_stable

let is_some x = not (is_none x)
let get_some_exn x = if is_none x then raise_s [%message "Moption.get_some_exn"] else !x
let set_some t v = t := v
let set_none t = t := none

let set t v =
  match v with
  | None -> set_none t
  | Some v -> set_some t v
;;

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    Option.iter (get t) ~f:invariant_a)
;;
