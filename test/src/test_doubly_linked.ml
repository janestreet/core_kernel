open! Core_kernel
open! Import
open! Doubly_linked

include Test_container.Test_S1 (Doubly_linked)

let%test_unit _ =
  List.iter
    [ [];
      [ 1 ];
      [ 2; 3 ];
    ]
    ~f:(fun l ->
      let sum = ref 0 in
      iter_elt (of_list l) ~f:(fun elt -> sum := !sum + Elt.value elt);
      assert (!sum = List.fold l ~init:0 ~f:(+)))
;;

let%test_module "move functions" =
  (module struct

    let n = 5

    let test k expected =
      let t = create () in
      let a = Array.init n ~f:(fun i -> insert_last t i) in
      k t a;
      invariant ignore t;
      assert (length t = n);
      let observed = to_list t in
      if observed <> expected then begin
        Error.failwiths "mismatch"
          (`Expected expected, `Observed observed)
          [%sexp_of: [`Expected of int list] *
                     [`Observed of int list]]
      end

    let%test_unit _ = test (fun _ _ -> ()) [0; 1; 2; 3; 4]

    let%test_unit _ = test (fun t a -> move_to_front t a.(4)) [4; 0; 1; 2; 3]
    let%test_unit _ = test (fun t a -> move_to_front t a.(3)) [3; 0; 1; 2; 4]
    let%test_unit _ = test (fun t a -> move_to_front t a.(2)) [2; 0; 1; 3; 4]
    let%test_unit _ = test (fun t a -> move_to_front t a.(1)) [1; 0; 2; 3; 4]
    let%test_unit _ = test (fun t a -> move_to_front t a.(0)) [0; 1; 2; 3; 4]

    let%test_unit _ = test (fun t a -> move_to_back  t a.(0)) [1; 2; 3; 4; 0]
    let%test_unit _ = test (fun t a -> move_to_back  t a.(1)) [0; 2; 3; 4; 1]
    let%test_unit _ = test (fun t a -> move_to_back  t a.(2)) [0; 1; 3; 4; 2]
    let%test_unit _ = test (fun t a -> move_to_back  t a.(3)) [0; 1; 2; 4; 3]
    let%test_unit _ = test (fun t a -> move_to_back  t a.(4)) [0; 1; 2; 3; 4]

    let%test_unit _ = test (fun t a -> move_before t a.(2) ~anchor:a.(1)) [0; 2; 1; 3; 4]
    let%test_unit _ = test (fun t a -> move_before t a.(2) ~anchor:a.(0)) [2; 0; 1; 3; 4]
    let%test_unit _ = test (fun t a -> move_before t a.(1) ~anchor:a.(0)) [1; 0; 2; 3; 4]
    let%test_unit _ = test (fun t a -> move_before t a.(0) ~anchor:a.(2)) [1; 0; 2; 3; 4]
    let%test_unit _ = test (fun t a -> move_before t a.(0) ~anchor:a.(1)) [0; 1; 2; 3; 4]
    let%test_unit _ = test (fun t a -> move_before t a.(3) ~anchor:a.(2)) [0; 1; 3; 2; 4]
    let%test_unit _ = test (fun t a -> move_before t a.(2) ~anchor:a.(3)) [0; 1; 2; 3; 4]

    let%test_unit _ = test (fun t a -> move_after  t a.(1) ~anchor:a.(3)) [0; 2; 3; 1; 4]
    let%test_unit _ = test (fun t a -> move_after  t a.(0) ~anchor:a.(2)) [1; 2; 0; 3; 4]
    let%test_unit _ = test (fun t a -> move_after  t a.(1) ~anchor:a.(4)) [0; 2; 3; 4; 1]
    let%test_unit _ = test (fun t a -> move_after  t a.(3) ~anchor:a.(2)) [0; 1; 2; 3; 4]
    let%test_unit _ = test (fun t a -> move_after  t a.(2) ~anchor:a.(3)) [0; 1; 3; 2; 4]
  end)

let%test _ = to_list (of_list []) = []
let%test _ = to_list (of_list [1;2;3]) = [1;2;3]
let%test _ = to_array (of_array [||]) = [||]
let%test _ = to_array (of_array [|1;2;3|]) = [|1;2;3|]

let%test_unit _ =
  invariant (fun (_:int) -> ()) (of_list []);
  invariant (fun (_:int) -> ()) (of_list [1;2;3]);
  invariant (fun (_:int) -> ()) (of_array [||]);
  invariant (fun (_:int) -> ()) (of_array [|1;2;3;|]);
;;

let%test _ =
  let t1 = create () in
  let t2 = create () in
  let elt = insert_first t1 15 in
  try
    remove t2 elt; false
  with
    _ -> true

let%test _ =
  let t1 = create () in
  let t2 = create () in
  let elt = insert_first t1 14 in
  let _   = insert_first t2 13 in
  try
    remove t2 elt; false
  with
    _ -> true

let%test_unit "mem_elt" =
  let t1 = create () in
  let a = insert_first t1 'a' in
  let b = insert_first t1 'b' in
  [%test_result: bool] (mem_elt t1 a) ~expect:true;
  [%test_result: bool] (mem_elt t1 b) ~expect:true;
  let t2 = create () in
  let b2 = insert_first t2 'b' in
  [%test_result: bool] (mem_elt t2 b2) ~expect:true;
  [%test_result: bool] (mem_elt t1 b2) ~expect:false;
  remove t1 a;
  [%test_result: bool] (mem_elt t1 a) ~expect:false;
  [%test_result: bool] (mem_elt t1 b) ~expect:true;
  remove t1 b;
  [%test_result: bool] (mem_elt t1 a) ~expect:false;
  [%test_result: bool] (mem_elt t1 b) ~expect:false;
;;

let%test_module "unchecked_iter" =
  (module struct
    let b = of_list [0; 1; 2; 3; 4]
    let element b n =
      Option.value_exn (find_elt b ~f:(fun value -> value = n))
    let remove b n =
      remove b (element b n)
    let insert_after b n_find n_add =
      ignore (insert_after b (element b n_find) n_add)
    let to_list f =
      let r = ref [] in
      let b = copy b in
      unchecked_iter b ~f:(fun n ->
        r := n :: !r;
        f b n;
      );
      List.rev !r
    let%test _ = to_list (fun _ _ -> ()) = [0; 1; 2; 3; 4]
    let%test _ = to_list (fun b x -> if x = 0 then remove b 1) = [0; 2; 3; 4]
    let%test _ = to_list (fun b x -> if x = 1 then remove b 0) = [0; 1; 2; 3; 4]
    let%test _ = to_list (fun b x -> if x = 2 then remove b 1) = [0; 1; 2; 3; 4]
    let%test _ = to_list (fun b x -> if x = 2 then begin remove b 4; remove b 3; end) = [0; 1; 2]
    let%test _ = to_list (fun b x -> if x = 2 then insert_after b 1 5) = [0; 1; 2; 3; 4]
    let%test _ = to_list (fun b x -> if x = 2 then insert_after b 2 5) = [0; 1; 2; 5; 3; 4]
    let%test _ = to_list (fun b x -> if x = 2 then insert_after b 3 5) = [0; 1; 2; 3; 5; 4]
  end)
