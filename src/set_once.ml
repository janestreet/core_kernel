open Bin_prot.Std
open Sexplib.Std
open Result

exception Already_set [@@deriving sexp]

type 'a t = 'a option ref [@@deriving bin_io, sexp]

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
