open Int_replace_polymorphic_compare
open Sexplib.Std

module List = Caml.ListLabels

type 'a t = Incl of 'a | Excl of 'a | Unbounded [@@deriving sexp]

type interval_comparison =
  | Below_lower_bound
  | In_range
  | Above_upper_bound
[@@deriving sexp, compare]

let map t ~f =
  match t with
  | Incl incl -> Incl (f incl)
  | Excl excl -> Excl (f excl)
  | Unbounded -> Unbounded

let is_lower_bound t ~of_:a ~compare =
  match t with
  | Incl incl -> compare incl a <= 0
  | Excl excl -> compare excl a <  0
  | Unbounded -> true

let%test_module "is_lower_bound" =
  (module struct
    let compare = Int_replace_polymorphic_compare.compare

    let%test _ = is_lower_bound Unbounded ~of_:min_int ~compare

    let%test _ = not (is_lower_bound (Incl 2) ~of_:1 ~compare)
    let%test _ =      is_lower_bound (Incl 2) ~of_:2 ~compare
    let%test _ =      is_lower_bound (Incl 2) ~of_:3 ~compare

    let%test _ = not (is_lower_bound (Excl 2) ~of_:1 ~compare)
    let%test _ = not (is_lower_bound (Excl 2) ~of_:2 ~compare)
    let%test _ =      is_lower_bound (Excl 2) ~of_:3 ~compare
  end)

let is_upper_bound t ~of_:a ~compare =
  match t with
  | Incl incl -> compare a incl <= 0
  | Excl excl -> compare a excl <  0
  | Unbounded -> true

let%test_module "is_upper_bound" =
  (module struct
    let compare = Int_replace_polymorphic_compare.compare

    let%test _ = is_upper_bound Unbounded ~of_:max_int ~compare

    let%test _ =      is_upper_bound (Incl 2) ~of_:1 ~compare
    let%test _ =      is_upper_bound (Incl 2) ~of_:2 ~compare
    let%test _ = not (is_upper_bound (Incl 2) ~of_:3 ~compare)

    let%test _ =      is_upper_bound (Excl 2) ~of_:1 ~compare
    let%test _ = not (is_upper_bound (Excl 2) ~of_:2 ~compare)
    let%test _ = not (is_upper_bound (Excl 2) ~of_:3 ~compare)
  end)

let check_interval_exn ~lower ~upper ~compare =
  match lower with
  | Unbounded -> ()
  | (Incl lower | Excl lower) ->
    match upper with
    | Unbounded -> ()
    | (Incl upper | Excl upper) ->
      if compare lower upper > 0 then
        failwith "Maybe_bound.compare_to_interval_exn: lower bound > upper bound"

let compare_to_interval_exn ~lower ~upper a ~compare =
  check_interval_exn ~lower ~upper ~compare;
  if not (is_lower_bound lower ~of_:a ~compare) then Below_lower_bound else
  if not (is_upper_bound upper ~of_:a ~compare) then Above_upper_bound else
    In_range

let interval_contains_exn ~lower ~upper a ~compare =
  match compare_to_interval_exn ~lower ~upper a ~compare with
  | In_range            -> true
  | Below_lower_bound
  | Above_upper_bound -> false

let%test_module "check_range" =
  (module struct
    let compare = Int_replace_polymorphic_compare.compare

    let tests (lower, upper) cases =
      List.iter cases ~f:(fun (n, comparison) ->
        [%test_result: interval_comparison]
          ~expect:comparison
          (compare_to_interval_exn n ~lower ~upper ~compare);
        [%test_result: bool]
          ~expect:(match comparison with In_range -> true | _ -> false)
          (interval_contains_exn n ~lower ~upper ~compare))

    let%test_unit _ =
      tests (Unbounded, Unbounded)
        [ (min_int, In_range)
        ; (0,       In_range)
        ; (max_int, In_range)
        ]

    let%test_unit _ =
      tests (Incl 2, Incl 4)
        [ (1, Below_lower_bound)
        ; (2, In_range)
        ; (3, In_range)
        ; (4, In_range)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Incl 2, Excl 4)
        [ (1, Below_lower_bound)
        ; (2, In_range)
        ; (3, In_range)
        ; (4, Above_upper_bound)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Excl 2, Incl 4)
        [ (1, Below_lower_bound)
        ; (2, Below_lower_bound)
        ; (3, In_range)
        ; (4, In_range)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Excl 2, Excl 4)
        [ (1, Below_lower_bound)
        ; (2, Below_lower_bound)
        ; (3, In_range)
        ; (4, Above_upper_bound)
        ; (5, Above_upper_bound)
        ]
  end)
