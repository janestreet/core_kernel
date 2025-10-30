type 'a t = 'a list =
  | []
  | ( :: ) of 'a * 'a t
[@@deriving equal ~localize]

(** Type alias for use within submodules *)
type 'a reversed_list = 'a t

open Base

let%template of_list_rev = (List.rev [@alloc a]) [@@alloc a = (stack, heap)]
let%template rev = (List.rev [@alloc a]) [@@alloc a = (stack, heap)]
let rev_append = List.rev_append
let rev_map = List.rev_map
let rev_filter_map = List.rev_filter_map
let is_empty = List.is_empty
let length = List.length

module O = struct
  type nonrec 'a t = 'a t =
    | []
    | ( :: ) of 'a * 'a t
end

module With_sexp_of = struct
  type nonrec 'a t = 'a t

  let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a t

  let%expect_test _ =
    Stdlib.print_endline (Sexp.to_string [%sexp ([ 1; 2 ] : int t)]);
    [%expect {| (1 2) |}]
  ;;
end

module With_rev_sexp_of = struct
  type nonrec 'a t = 'a t

  let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (rev t)

  let%expect_test _ =
    Stdlib.print_endline (Sexp.to_string [%sexp ([ 1; 2 ] : int t)]);
    [%expect {| (2 1) |}]
  ;;
end

(* Functions that operate between Reversed_list.t and Nonempty_list.t *)

let rec rev_append_to_nonempty xs acc =
  match (xs : _ t) with
  | [] -> acc
  | hd :: tl -> rev_append_to_nonempty tl (Nonempty_list.cons hd acc)
;;

(* Non-empty version of Reversed_list.t *)
module Nonempty = struct
  type 'a nonempty_list = 'a Nonempty_list.t
  type 'a t = ( :: ) of 'a * 'a reversed_list

  let to_rev_list (hd :: tl) : _ reversed_list = hd :: tl
  let rev_append (hd :: tl) xs = rev_append_to_nonempty tl (hd :: xs)
  let rev t = rev_append t []

  let rec rev_map_aux i xs ~f acc =
    match (xs : _ reversed_list) with
    | [] -> acc
    | hd :: tl -> rev_map_aux (i + 1) tl ~f (Nonempty_list.cons (f i hd) acc)
  ;;

  let rev_mapi (hd :: tl : _ t) ~f = rev_map_aux 1 tl ~f ([ f 0 hd ] : _ nonempty_list)
  let rev_map t ~f = rev_mapi t ~f:(fun _ x -> f x) [@nontail]
  let cons x t = x :: to_rev_list t

  module With_sexp_of = struct
    type nonrec 'a t = 'a t

    let sexp_of_t sexp_of_a t = With_sexp_of.sexp_of_t sexp_of_a (to_rev_list t)
  end

  module With_rev_sexp_of = struct
    type nonrec 'a t = 'a t

    let sexp_of_t sexp_of_a t = With_rev_sexp_of.sexp_of_t sexp_of_a (to_rev_list t)
  end
end

let of_nonempty (hd :: tl : _ Nonempty_list.t) =
  List.fold tl ~init:([ hd ] : _ Nonempty.t) ~f:(Fn.flip Nonempty.cons)
;;
