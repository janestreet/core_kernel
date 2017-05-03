module Stable = struct
  open Stable_internal

  module V1 = struct
    type 'a t = 'a option ref [@@deriving bin_io, sexp]
  end
end

open! Import

exception Already_set [@@deriving sexp]

module Unstable = Stable.V1

type 'a t = 'a Unstable.t [@@deriving sexp_of]

let invariant invariant_a t =
  match !t with
  | None -> ()
  | Some a -> invariant_a a
;;

let create () = ref None

let set_exn t v =
  match !t with
  | None -> t := Some v
  | Some _ -> raise Already_set

let set t v =
  match !t with
  | None -> t := Some v; Ok ()
  | Some _ -> Error "already set"

let get t = !t

let get_exn t = Option.value_exn !t
