open Std_internal

module Id = Type_equal.Id

module View = struct
  type t = T : 'a Id.t * 'a -> t
end

include View

let view = Fn.id

let create id value = T (id, value)

let type_id_name (T (id, _)) = Id.name id

let type_id_uid (T (id, _)) = Id.uid id

let sexp_of_t (T (id, value)) = Id.to_sexp id value

let does_match (T (id1, _)) id2 = Id.same id1 id2

let match_ (type a) (T (id1, value)) (id2 : a Id.t) =
  if not (Id.same id1 id2)
  then None
  else
    let Type_equal.T = Id.same_witness_exn id1 id2 in
    Some (value : a)
;;

let match_exn (type a) (T (id1, value) as t) (id2 : a Id.t) =
  if not (Id.same id1 id2)
  then failwiths "Univ.match_exn called with mismatched value and type id" (t, id2)
         (<:sexp_of< t * _ Id.t >>)
  else
    let Type_equal.T = Id.same_witness_exn id1 id2 in
    (value : a)
;;

TEST_MODULE = struct

  let c1 = Id.create ~name:"c1" Int.sexp_of_t
  let c2 = Id.create ~name:"c2" Int.sexp_of_t
  let t1 = create c1 13
  let t2 = create c2 13

  TEST_UNIT = ignore (<:sexp_of< _ Id.t >> c1 : Sexp.t)
  TEST_UNIT = ignore (<:sexp_of< t >> t1 : Sexp.t)

  TEST = type_id_name t1 = Id.name c1

  TEST = does_match t1 c1
  TEST = not (does_match t1 c2)
  TEST = not (does_match t2 c1)
  TEST = does_match t2 c2

  TEST =
    match match_ t1 c1 with
    | None -> false
    | Some v -> v = 13
  ;;

  TEST = Option.is_none (match_ t1 c2)

  TEST = match_exn t1 c1 = 13
  TEST = Result.is_error (Result.try_with (fun () -> match_exn t1 c2))

end
