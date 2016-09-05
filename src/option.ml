open! Import
open Typerep_lib.Std
open Sexplib.Std
open Bin_prot.Std
open Hash.Builtin
module List = ListLabels

type 'a t = 'a option [@@deriving bin_io, sexp, compare, hash, typerep]

let is_none = function None -> true | _ -> false

let is_some = function Some _ -> true | _ -> false

let value_map o ~default ~f =
  match o with
  | Some x -> f x
  | None   -> default

let iter o ~f =
  match o with
  | None -> ()
  | Some a -> f a
;;

let invariant f t = iter t ~f

let map2 o1 o2 ~f =
  match o1, o2 with
  | Some a1, Some a2 -> Some (f a1 a2)
  | _ -> None

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x

let value t ~default =
  match t with
  | None -> default
  | Some x -> x
;;

let (|?) t default =
    value t ~default
;;

let value_exn ?here ?error ?message t =
  match t with
  | Some x -> x
  | None ->
    let error =
      match here, error, message with
      | None  , None  , None   -> Error.of_string "Option.value_exn None"
      | None  , None  , Some m -> Error.of_string m
      | None  , Some e, None   -> e
      | None  , Some e, Some m -> Error.tag e ~tag:m
      | Some p, None  , None   ->
        Error.create "Option.value_exn" p [%sexp_of: Source_code_position0.t]
      | Some p, None  , Some m ->
        Error.create m p [%sexp_of: Source_code_position0.t]
      | Some p, Some e, _      ->
        Error.create (value message ~default:"") (e, p)
          [%sexp_of: Error.t * Source_code_position0.t]
    in
    Error.raise error
;;

let to_array t =
  match t with
  | None -> [||]
  | Some x -> [|x|]
;;

let to_list t =
  match t with
  | None -> []
  | Some x -> [x]
;;

let min_elt t ~cmp:_ = t
let max_elt t ~cmp:_ = t
let sum (type a) (module M : Commutative_group.S with type t = a) t ~f =
  match t with
  | None -> M.zero
  | Some x -> f x
;;

let for_all t ~f =
  match t with
  | None -> true
  | Some x -> f x
;;

let exists t ~f =
  match t with
  | None -> false
  | Some x -> f x
;;

let mem ?(equal = (=)) t a =
  match t with
  | None -> false
  | Some a' -> equal a a'
;;

let length t =
  match t with
  | None -> 0
  | Some _ -> 1
;;

let is_empty = is_none

let fold t ~init ~f =
  match t with
  | None -> init
  | Some x -> f init x
;;

let count t ~f =
  match t with
  | None -> 0
  | Some a -> if f a then 1 else 0
;;

let find t ~f =
  match t with
  | None -> None
  | Some x -> if f x then Some x else None
;;

let find_map t ~f =
  match t with
  | None -> None
  | Some a -> f a
;;

let equal f t t' =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false

let some x = Some x

let both x y =
  match x,y with
  | Some a, Some b -> Some (a,b)
  | _ -> None

let first_some x y =
  match x with
  | Some _ -> x
  | None -> y

let some_if cond x = if cond then Some x else None

let merge a b ~f =
  match a, b with
  | None, x | x, None -> x
  | Some a, Some b -> Some (f a b)

let%test_module _ = (module struct
  let f = (+)
  let%test _ = merge None None ~f  = None
  let%test _ = merge (Some 3) None ~f = Some 3
  let%test _ = merge None (Some 3) ~f = Some 3
  let%test _ = merge (Some 1) (Some 3) ~f = (Some 4)
end)

let filter t ~f =
  match t with
  | Some v as o when f v -> o
  | _ -> None

let try_with f =
  try Some (f ())
  with _ -> None

include Monad.Make (struct
  type 'a t = 'a option
  let return x = Some x
  let map t ~f =
    match t with
    | None -> None
    | Some a -> Some (f a)
  ;;
  let map = `Custom map
  let bind o ~f =
    match o with
    | None -> None
    | Some x -> f x
end)

let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until  t ~init ~f = Container.fold_until  ~fold ~init ~f t

let validate ~none ~some t =
  let module V = Validate in
  match t with
  | None   -> V.name "none" (V.protect none ())
  | Some x -> V.name "some" (V.protect some x )
;;

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Monad_infix

  let gen elt_gen =
    Generator.union
      [ Generator.singleton None
      ; elt_gen >>| return
      ]

  let obs elt_obs =
    Observer.unmap (Observer.variant2 (Observer.singleton ()) elt_obs)
      ~f:(function
        | None   -> `A ()
        | Some x -> `B x)

  let shrinker elt_shr =
    let shrinker = function
      | Some elt ->
        Sequence.append
          (Sequence.singleton None)
          (Sequence.map (Shrinker.shrink elt_shr elt) ~f:(fun v -> Some v))
      | None ->
        Sequence.empty
    in
    Shrinker.create shrinker

  let%test_module "shrinker" =
    (module struct

      let t1 = Shrinker.create (Fn.const (Sequence.singleton 1))

      let%test_unit _ =
        [%test_result: int option list]
          (Sequence.to_list (Shrinker.shrink (shrinker t1) None))
          ~expect:[]

      let%test_unit _ =
        let sort = List.sort ~cmp:[%compare: int option ] in
        let expect =
          [ None; Some 1]
          |> sort
        in
        let results =
          Shrinker.shrink (shrinker t1) (Some 5)
          |> Sequence.to_list
          |> sort
        in
        [%test_result: int option list ] ~expect results

    end)

end

let gen      = For_quickcheck.gen
let obs      = For_quickcheck.obs
let shrinker = For_quickcheck.shrinker
