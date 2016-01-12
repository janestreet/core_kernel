module Stable = struct
  module V1 = struct
    type ('f, 's) t =
      | First  of 'f
      | Second of 's
    [@@deriving bin_io, compare, sexp, typerep]

    let map x ~f1 ~f2 =
      match x with
      | First  x1 -> First  (f1 x1)
      | Second x2 -> Second (f2 x2)
  end
end

include Stable.V1

let swap = function
  | First  x -> Second x
  | Second x -> First  x
;;

let is_first = function
  | First  _ -> true
  | Second _ -> false
;;

let is_second = function
  | First  _ -> false
  | Second _ -> true
;;

let value (First x | Second x) = x
;;

let value_map t ~first ~second =
  match t with
  | First x -> first x
  | Second x -> second x
;;

let iter = value_map
;;

let map t ~first ~second =
  match t with
  | First x -> First (first x)
  | Second x -> Second (second x)
;;

let first  x = First  x
let second x = Second x
;;

let equal eq1 eq2 t1 t2 =
  match t1, t2 with
  | First  x, First  y -> eq1 x y
  | Second x, Second y -> eq2 x y
  | First  _, Second _
  | Second _, First  _ -> false
;;

let invariant f s = function
  | First  x -> f x
  | Second y -> s y
;;

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Monad_infix

  let to_poly = function
    | First  a -> `A a
    | Second b -> `B b

  let of_poly = function
    | `A a -> First a
    | `B b -> Second b

  let gen a b =
    Generator.union
      [ a >>| first
      ; b >>| second
      ]

  let obs a b =
    Observer.unmap (Observer.variant2 a b)
      ~f:to_poly
      ~f_sexp:(fun () -> Atom "variant_of_either")

  let shrinker a b =
    Shrinker.map
      (Shrinker.variant2 a b)
      ~f:of_poly
      ~f_inverse:to_poly

end

let gen      = For_quickcheck.gen
let obs      = For_quickcheck.obs
let shrinker = For_quickcheck.shrinker

module Make_focused (M : sig
    type (+'a, +'b) t

    val return : 'a -> ('a, _) t
    val other  : 'b -> (_, 'b) t

    val either  : ('a, 'b) t -> return:('a -> 'c) -> other:('b -> 'c) -> 'c

    val combine
      :  ('a, 'd) t
      -> ('b, 'd) t
      -> f:('a -> 'b -> 'c)
      -> other:('d -> 'd -> 'd)
      -> ('c, 'd) t
  end) = struct
  include M
  open With_return

  let map t ~f = either t ~return:(fun x -> return (f x)) ~other

  include Monad.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let return = return
      ;;

      let bind t f = either t ~return:f ~other
      ;;

      let map = `Custom map
    end)

  module App = Applicative.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let return = return
      ;;

      let apply t1 t2 =
        let return f = either t2 ~return:(fun x -> return (f x)) ~other in
        either t1 ~return ~other
      ;;

      let map = `Custom map
    end)

  include App

  module Args = Applicative.Make_args2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t
      include App
    end)

  let combine_all =
    let rec other_loop f acc = function
      | [] -> other acc
      | t :: ts ->
        either t ~return:(fun _ -> other_loop f acc ts)
          ~other:(fun o -> other_loop f (f acc o) ts)
    in
    let rec return_loop f acc = function
      | [] -> return (List.rev acc)
      | t :: ts ->
        either t ~return:(fun x -> return_loop f (x :: acc) ts)
          ~other:(fun o -> other_loop f o ts)
    in
    fun ts ~f -> return_loop f [] ts
  ;;

  let combine_all_unit =
    let rec other_loop f acc = function
      | [] -> other acc
      | t :: ts ->
        either t ~return:(fun () -> other_loop f acc ts)
          ~other:(fun o -> other_loop f (f acc o) ts)
    in
    let rec return_loop f = function
      | [] -> return ()
      | t :: ts ->
        either t ~return:(fun () -> return_loop f ts)
          ~other:(fun o -> other_loop f o ts)
    in
    fun ts ~f -> return_loop f ts
  ;;

  let iter t ~f = either t ~return:f ~other:(fun _ -> ())
  ;;

  let to_option t = either t ~return:Option.some ~other:(fun _ -> None)
  ;;

  let fold t ~init ~f = either t ~return:(fun x -> f init x) ~other:(fun _ -> init)
  ;;

  let value t ~default = either t ~return:(fun x -> x) ~other:(fun _ -> default)
  ;;

  let count t ~f = fold t ~init:0 ~f:(fun n a -> if f a then n + 1 else n)
  ;;

  let sum (type a) (module M : Commutative_group.S with type t = a) t ~f =
    fold t ~init:M.zero ~f:(fun n a -> M.(+) n (f a))
  ;;

  let length c = fold c ~init:0 ~f:(fun acc _ -> acc + 1)
  ;;

  let is_empty c =
    with_return (fun r ->
      iter c ~f:(fun _ -> r.return false);
      true)
  ;;

  let exists c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return true);
      false)
  ;;

  let mem ?(equal = (=)) t a = exists t ~f:(equal a)
  ;;

  let for_all c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if not (f x) then r.return false);
      true)
  ;;

  let find_map t ~f =
    with_return (fun r ->
      iter t ~f:(fun x -> match f x with None -> () | Some _ as res -> r.return res);
      None)
  ;;

  let find c ~f =
    with_return (fun r ->
      iter c ~f:(fun x -> if f x then r.return (Some x));
      None)
  ;;

  let to_list c = List.rev (fold c ~init:[] ~f:(fun acc x -> x :: acc))
  ;;

  let to_array c = Array.of_list (to_list c)
  ;;

  let min_elt t ~cmp =
    fold t ~init:None ~f:(fun acc elt ->
      match acc with
      | None -> Some elt
      | Some min -> if cmp min elt > 0 then Some elt else acc)
  ;;

  let max_elt t ~cmp =
    fold t ~init:None ~f:(fun acc elt ->
      match acc with
      | None -> Some elt
      | Some max -> if cmp max elt < 0 then Some elt else acc)
  ;;

  let with_return f =
    with_return (fun ret ->
      other (f (With_return.prepend ret ~f:return)))
  ;;

end

module First = Make_focused (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let return = first
    let other  = second
    ;;

    let either t ~return ~other =
      match t with
      | First  x -> return x
      | Second y -> other  y
    ;;

    let combine t1 t2 ~f ~other =
      match t1, t2 with
      | First x, First y -> First (f x y)
      | Second x, Second y -> Second (other x y)
      | Second x, _
      | _, Second x -> Second x
  end)

module Second = Make_focused (struct
    type nonrec ('a, 'b) t = ('b, 'a) t

    let return = second
    let other  = first
    ;;

    let either t ~return ~other =
      match t with
      | Second y -> return y
      | First  x -> other  x
    ;;

    let combine t1 t2 ~f ~other =
      match t1, t2 with
      | Second x, Second y -> Second (f x y)
      | First x, First y -> First (other x y)
      | First x, _
      | _, First x -> First x
  end)

module Export = struct
  type ('f, 's) _either
    = ('f, 's) t
    = First  of 'f
    | Second of 's
end
