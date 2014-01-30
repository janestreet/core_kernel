open Sexplib
open Sexplib.Conv

module Debug (Core_queue : module type of Core_queue) = struct

  module Debug = Debug.Make (struct end)

  include Debug

  open Core_queue

  type nonrec 'a t = 'a t with bin_io, sexp

  let invariant = invariant

  let debug x = debug (invariant ignore) ~module_name:"Core_queue" x

  let mem ?equal t elt =
    debug "mem" [ t ] t <:sexp_of< _ t >> <:sexp_of< bool >>
      (fun () -> mem ?equal t elt)
  ;;

  let length t =
    debug "length" [ t ] t <:sexp_of< _ t >> <:sexp_of< int >>
      (fun () -> length t)
  ;;

  let is_empty t =
    debug "is_empty" [ t ] t <:sexp_of< _ t >> <:sexp_of< bool >>
      (fun () -> is_empty t)
  ;;

  let iter t ~f =
    debug "iter" [ t ] t <:sexp_of< _ t >> <:sexp_of< unit >>
      (fun () -> iter t ~f)
  ;;

  let fold t ~init ~f =
    debug "fold" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ >>
      (fun () -> fold t ~init ~f)
  ;;

  let exists t ~f =
    debug "exists" [ t ] t <:sexp_of< _ t >> <:sexp_of< bool >>
      (fun () -> exists t ~f)
  ;;

  let for_all t ~f =
    debug "for_all" [ t ] t <:sexp_of< _ t >> <:sexp_of< bool >>
      (fun () -> for_all t ~f)
  ;;

  let count t ~f =
    debug "count" [ t ] t <:sexp_of< _ t >> <:sexp_of< int >>
      (fun () -> count t ~f)
  ;;

  let find t ~f =
    debug "find" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ option >>
      (fun () -> find t ~f)
  ;;

  let find_map t ~f =
    debug "find_map" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ option >>
      (fun () -> find_map t ~f)
  ;;

  let to_list t =
    debug "to_list" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ list >>
      (fun () -> to_list t)
  ;;

  let to_array t =
    debug "to_array" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ array >>
      (fun () -> to_array t)
  ;;

  let create ?capacity () =
    debug "create" [ ] capacity <:sexp_of< int option >> <:sexp_of< _ t >>
      (fun () -> create ?capacity ())
  ;;

  let singleton a =
    debug "singleton" [ ] () <:sexp_of< unit >> <:sexp_of< _ t >>
      (fun () -> singleton a)
  ;;

  let enqueue t a =
    debug "enqueue" [ t ] t <:sexp_of< _ t >> <:sexp_of< unit >>
      (fun () -> enqueue t a)
  ;;

  let dequeue t =
    debug "dequeue" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ option >>
      (fun () -> dequeue t)
  ;;

  let dequeue_exn t =
    debug "dequeue_exn" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ >>
      (fun () -> dequeue_exn t)
  ;;

  let peek t =
    debug "peek" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ option >>
      (fun () -> peek t)
  ;;

  let peek_exn t =
    debug "peek_exn" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ >>
      (fun () -> peek_exn t)
  ;;

  let clear t =
    debug "clear" [ t ] t <:sexp_of< _ t >> <:sexp_of< unit >>
      (fun () -> clear t)
  ;;

  let copy t =
    debug "copy" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ t >>
      (fun () -> copy t)
  ;;

  let blit_transfer ~src ~dst ?len () =
    debug "blit_transfer" [ src; dst ] (src, dst, len)
      <:sexp_of< _ t * _ t * int option >> <:sexp_of< unit >>
      (fun () -> blit_transfer ~src ~dst ?len ())
  ;;

  let of_list l =
    debug "of_list" [ ] l <:sexp_of< _ list >> <:sexp_of< _ t >>
      (fun () -> of_list l)
  ;;

  let map t ~f =
    debug "map" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ t >>
      (fun () -> map t ~f)
  ;;

  let concat_map t ~f =
    debug "concat_map" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ t >>
      (fun () -> concat_map t ~f)
  ;;

  let filter_map t ~f =
    debug "filter_map" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ t >>
      (fun () -> filter_map t ~f)
  ;;

  let filter t ~f =
    debug "filter" [ t ] t <:sexp_of< _ t >> <:sexp_of< _ t >>
      (fun () -> filter t ~f)
  ;;

  let filter_inplace t ~f =
    debug "filter_inplace" [ t ] t <:sexp_of< _ t >> <:sexp_of< unit >>
      (fun () -> filter_inplace t ~f)
  ;;

  let of_array a =
    debug "of_array" [ ] a <:sexp_of< _ array >> <:sexp_of< _ t >>
      (fun () -> of_array a)
  ;;

  let get t i =
    debug "get" [ t ] (t, i) <:sexp_of< _ t * int >> <:sexp_of< _ >>
      (fun () -> get t i)
  ;;

  let set t i a =
    debug "set" [ t ] (t, i) <:sexp_of< _ t * int >> <:sexp_of< unit >>
      (fun () -> set t i a)
  ;;

  let capacity t =
    debug "capacity" [ t ] t <:sexp_of< _ t >> <:sexp_of< int >>
      (fun () -> capacity t)
  ;;

  let set_capacity t capacity =
    debug "set_capacity" [ t ] (t, capacity) <:sexp_of< _ t * int >> <:sexp_of< unit >>
      (fun () -> set_capacity t capacity)
  ;;
end
