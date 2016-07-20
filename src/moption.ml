open Std_internal

type 'a t = 'a Option_array.t

let the_index = 0
let the_length = 1

let is_some t = Option_array.unsafe_is_some t the_index
let is_none t = not (is_some t)

let get t = Option_array.unsafe_get t the_index

let get_some_exn t =
  if is_none t then failwith "Moption.get_exn";
  Option_array.unsafe_get_some_exn t the_index;
;;

let sexp_of_t sexp_of_a t = [%sexp (get t : a option)]

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    Option.iter (get t) ~f:invariant_a;
    [%test_result: int] (Option_array.length t) ~expect:the_length)
;;

let create () = Option_array.create ~len:the_length

let set t o = Option_array.unsafe_set t the_index o

let set_some t a = Option_array.unsafe_set_some t the_index a
let set_none t   = Option_array.unsafe_set_none t the_index
