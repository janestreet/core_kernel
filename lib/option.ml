open Typerep_lib.Std
open Sexplib.Std
open Bin_prot.Std

type 'a t = 'a option with bin_io, sexp, compare, typerep

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

let map2 o1 o2 ~f =
  match o1, o2 with
  | Some a1, Some a2 -> Some (f a1 a2)
  | _ -> None

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x

let apply x ~f =
  match f with
  | None -> None
  | Some f -> Some (f x)

let value t ~default =
  match t with
  | None -> default
  | Some x -> x
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
      | None  , Some e, Some m -> Error.tag e m
      | Some p, None  , None   ->
        Error.create "Option.value_exn" p <:sexp_of< Source_code_position0.t_hum >>
      | Some p, None  , Some m ->
        Error.create m p <:sexp_of< Source_code_position0.t_hum >>
      | Some p, Some e, _      ->
        Error.create (value message ~default:"") (e, p)
          <:sexp_of< Error.t * Source_code_position0.t_hum >>
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

TEST_MODULE = struct
  let f = (+)
  TEST = merge None None ~f  = None
  TEST = merge (Some 3) None ~f = Some 3
  TEST = merge None (Some 3) ~f = Some 3
  TEST = merge (Some 1) (Some 3) ~f = (Some 4)
end

let filter ~f = function
  | Some v as o when f v -> o
  | _ -> None

let compare ~cmp v1 v2 = compare cmp v1 v2

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
  let bind o f =
    match o with
    | None -> None
    | Some x -> f x
end)

let validate ~none ~some t =
  let module V = Validate in
  match t with
  | None   -> V.name "none" (V.protect none ())
  | Some x -> V.name "some" (V.protect some x )
;;
