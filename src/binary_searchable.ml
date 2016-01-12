let polymorphic_compare = (=)
open Int_replace_polymorphic_compare
include Binary_searchable_intf

module type Arg_without_tests = sig
  type 'a elt
  type 'a t
  val get : 'a t -> int -> 'a elt
  val length : _ t -> int
end

module type Arg = sig
  include Arg_without_tests
  module For_test : sig
    val small    : bool elt
    val big      : bool elt
    val of_array : bool elt array -> bool t
  end
end

module Make_gen_without_tests (T : Arg_without_tests) = struct

  (* These functions implement a search for the first (resp. last) element
     satisfying a predicate, assuming that the predicate is increasing on
     the container, meaning that, if the container is [u1...un], there exists a
     k such that p(u1)=....=p(uk) = false and p(uk+1)=....=p(un)= true.
     If this k = 1 (resp n), find_last_not_satisfaying (resp find_first_satisfying)
     will return None.
  *)

  let rec linear_search_first_satisfying t ~lo ~hi ~pred =
    if lo > hi
    then None
    else
      if pred (T.get t lo)
      then Some lo
      else linear_search_first_satisfying  t ~lo:(lo + 1) ~hi ~pred
  ;;

  (* Takes a container [t], a predicate [pred] and two indices [lo < hi], such that
     [pred] is increasing on [t] between [lo] and [hi].

     return a range (lo, hi) where:
     - lo and hi are close enough together for a linear search
     - If [pred] is not constantly [false] on [t] between [lo] and [hi], the first element
       on which [pred] is [true] is between [lo] and [hi].

  *)
  (* Invariant: the first element satisfying [pred], if it exists is between [lo] and [hi]*)
  let rec find_range_near_first_satisfying t ~lo ~hi ~pred =
    (* Warning: this function will not terminate if the constant (currently 8) is
       set <= 1 *)
    if hi - lo <= 8
    then (lo,hi)
    else
      let mid = lo + ((hi - lo) / 2) in
      if pred (T.get t mid)
    (* INVARIANT check: it means the first satisfying element is between [lo] and [mid] *)
      then find_range_near_first_satisfying t ~lo ~hi:mid ~pred
    (* INVARIANT check: it means the first satisfying element, if it exists,
      is between [mid+1] and [hi] *)
      else find_range_near_first_satisfying t ~lo:(mid+1) ~hi ~pred
  ;;

  let find_first_satisfying ?pos ?len t ~pred =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(T.length t)
    in
    let lo     = pos in
    let hi     = pos + len - 1 in
    let (lo, hi) = find_range_near_first_satisfying t ~lo ~hi ~pred in
    linear_search_first_satisfying t ~lo ~hi ~pred
  ;;

  (* Takes an array with shape [true,...true,false,...false] (i.e., the _reverse_ of what
     is described above) and returns the index of the last true or None if there are no
     true*)
  let find_last_satisfying ?pos ?len t ~pred =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(T.length t)
    in
    if len = 0
    then None
    else begin
      (* The last satisfying is the one just before the first not satisfying *)
      match find_first_satisfying ~pos ~len t ~pred:(fun x -> not (pred x)) with
      | None -> Some (pos + len - 1) (* This means that all elements satisfy pred.
                                        There is at least an element as (len > 0) *)
      | Some i when i = pos -> None (* no element satisfies pred *)
      | Some i -> Some (i - 1)
    end
  ;;

  let binary_search ?pos ?len t ~compare how v =
    match how with
    | `Last_strictly_less_than ->
      find_last_satisfying ?pos ?len t ~pred:(fun x -> compare x v < 0)
    | `Last_less_than_or_equal_to ->
      find_last_satisfying ?pos ?len t ~pred:(fun x -> compare x v <= 0)
    | `First_equal_to ->
      begin
        match find_first_satisfying ?pos ?len t ~pred:(fun x -> compare x v >= 0) with
        | Some x when compare (T.get t x) v = 0 -> Some x
        | None | Some _ -> None
      end
    | `Last_equal_to ->
      begin
        match find_last_satisfying ?pos ?len t ~pred:(fun x -> compare x v <= 0) with
        | Some x when compare (T.get t x) v = 0 -> Some x
        | None | Some _ -> None
      end
    | `First_greater_than_or_equal_to ->
      find_first_satisfying ?pos ?len t ~pred:(fun x -> compare x v >= 0)
    | `First_strictly_greater_than ->
      find_first_satisfying ?pos ?len t ~pred:(fun x -> compare x v > 0)
  ;;

  let binary_search_segmented ?pos ?len t ~segment_of how =
    let is_left x =
      match segment_of x with
      | `Left -> true
      | `Right -> false
    in
    let is_right x = not (is_left x) in
    match how with
    | `Last_on_left -> find_last_satisfying ?pos ?len t ~pred:is_left
    | `First_on_right -> find_first_satisfying ?pos ?len t ~pred:is_right
  ;;

end

module Make_gen (T : Arg) = struct

  include Make_gen_without_tests (T)

  let%test_module "test_binary_searchable" = (module struct
    let compare x y =
      if x == y then 0 else
      if x == T.For_test.small then -1 else 1

    let elt_compare = compare

    let s = T.For_test.small
    let b = T.For_test.big

    let binary_search ?pos ?len ~compare t how v =
      binary_search ?pos ?len ~compare (T.For_test.of_array t) how v

    let (=) = polymorphic_compare

    let%test _ = binary_search ~compare [|          |]  `First_equal_to s = None
    let%test _ = binary_search ~compare [| s        |]  `First_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s        |]  `First_equal_to b = None
    let%test _ = binary_search ~compare [| s ; b    |]  `First_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s ; b    |]  `First_equal_to b = Some 1
    let%test _ = binary_search ~compare [| b ; b    |]  `First_equal_to s = None
    let%test _ = binary_search ~compare [| s ; s    |]  `First_equal_to b = None
    let%test _ = binary_search ~compare [| s ; b ; b |] `First_equal_to b = Some 1
    let%test _ = binary_search ~compare [| s ; s ; b |] `First_equal_to s = Some 0
    let%test _ = binary_search ~compare [| b ; b ; b |] `First_equal_to s = None

    let%test _ = binary_search ~compare [|          |]  `Last_equal_to s = None
    let%test _ = binary_search ~compare [| s        |]  `Last_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s        |]  `Last_equal_to b = None
    let%test _ = binary_search ~compare [| s ; b    |]  `Last_equal_to b = Some 1
    let%test _ = binary_search ~compare [| s ; b    |]  `Last_equal_to s = Some 0
    let%test _ = binary_search ~compare [| b ; b    |]  `Last_equal_to s = None
    let%test _ = binary_search ~compare [| s ; s    |]  `Last_equal_to b = None
    let%test _ = binary_search ~compare [| s ; b ; b |] `Last_equal_to b = Some 2
    let%test _ = binary_search ~compare [| s ; s ; b |] `Last_equal_to s = Some 1
    let%test _ = binary_search ~compare [| b ; b; b |]  `Last_equal_to s = None

    let%test _ = binary_search ~compare [||] `First_greater_than_or_equal_to s    = None
    let%test _ = binary_search ~compare [| b |] `First_greater_than_or_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s |] `First_greater_than_or_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s |] `First_strictly_greater_than s    = None

    let%test _ = binary_search ~compare [||] `Last_less_than_or_equal_to  s   = None
    let%test _ = binary_search ~compare [| b |] `Last_less_than_or_equal_to s = None
    let%test _ = binary_search ~compare [| s |] `Last_less_than_or_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s |] `Last_strictly_less_than s = None

    let create_test_case (num_s, num_b) =
      let arr = Array.make (num_s + num_b) b in
      for i = 0 to num_s -1 do
        arr.(i) <- s
      done;
      arr
    ;;

    let only_small   = (10_000, 0)
    let only_big   =  (0, 10_000)

    let both = (2531, 4717)

    let%test _ = Option.is_some (binary_search (create_test_case only_small) ~compare `First_equal_to s)

    let%test _ =
      let arr = create_test_case both in
      match binary_search arr ~compare `First_equal_to b with
      | None -> false
      | Some v -> v = 2531

    let%test _ =
      let arr = create_test_case only_small in
      binary_search arr ~compare `First_equal_to b = None

    let create_deterministic_test () =
      Array.init 100_000 (fun i -> if i > 50_000 then b else s)

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_equal_to s = Some 0

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_equal_to s = Some 50_000

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_greater_than_or_equal_to s = Some 0

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_less_than_or_equal_to s = Some 50_000

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_strictly_greater_than s = Some 50_001

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_strictly_less_than b = Some 50_000

    (* tests around a gap*)
    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_equal_to b  = Some 50_001

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_equal_to b = Some 99_999

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_greater_than_or_equal_to b = Some 50_001

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_less_than_or_equal_to b = Some 99_999

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_strictly_greater_than b = None

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_strictly_less_than b = Some 50_000

    (* test beginning of array *)

    let%test _ =
      let arr = create_test_case only_big in
      binary_search arr ~compare `First_equal_to s  = None

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `Last_equal_to s = None

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `First_greater_than_or_equal_to s = Some 0

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `Last_less_than_or_equal_to s = None

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `First_strictly_greater_than s = Some 0

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `Last_strictly_less_than b = None


    (* test end of array *)

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `First_equal_to b  = None

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `Last_equal_to b = None

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `First_greater_than_or_equal_to b = None

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `Last_less_than_or_equal_to b = Some 9_999

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `First_strictly_greater_than s = None

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `Last_strictly_less_than b = Some 9_999

    let%test_unit _ =
      let open Result in
      for length = 0 to 5 do
        for num_s = 0 to length do
          let arr = Array.init length (fun i -> if i < num_s then s else b) in
          for pos = -1 to length do
            for len = -1 to length + 1 do
              (*try*)
              let should_raise =
                Exn.does_raise (fun () ->
                  Ordered_collection_common.check_pos_len_exn ~pos ~len ~length)
              in
              let result =
                Or_error.try_with (fun () ->
                  binary_search arr  ~pos ~len ~compare:elt_compare `Last_equal_to s)
              in
              match should_raise, result with
              | true , Error _   -> ()
              | true , Ok _      -> failwith "expected it to raise but it didn't"
              | false, Error _   -> failwith "expected it to not raise, but it raised"
              | false, Ok result ->
                let searched = num_s - 1 in
                let correct_result =
                  if searched < pos then None
                  else if len = 0 then None
                  else if searched >= pos + len then Some(pos + len - 1)
                  else Some searched
                in
                if not (correct_result = result) then failwith "Wrong result"
                (*with exn ->
                  failwiths "binary_search bug"
                  (exn, `length length, `search_key search_key, `pos pos, `len len)
                  <:sexp_of< exn * [ `length of int ] * [ `search_key of int ]
                 * [ `pos of int ] * [ `len of int ] >>*)
            done;
          done;
        done;
      done
    ;;

    let binary_search_segmented a = binary_search_segmented (T.For_test.of_array a)

    (*test for binary_search_segmented*)
    let%test _ =
      let arr = create_deterministic_test () in
      let segment_of x = if x = b then `Right else `Left in
      binary_search_segmented arr ~segment_of `Last_on_left    = Some 50_000 &&
      binary_search_segmented arr ~segment_of `First_on_right  = Some 50_001

    let%test _ =
      let arr = create_deterministic_test () in
      let segment_of _ = `Right in
      binary_search_segmented arr ~segment_of `Last_on_left    = None &&
      binary_search_segmented arr ~segment_of `First_on_right  = Some 0

    let%test _ =
      let arr = create_deterministic_test () in
      let segment_of _ = `Left in
      binary_search_segmented arr ~segment_of `Last_on_left    = Some 99_999 &&
      binary_search_segmented arr ~segment_of `First_on_right  = None

  end)

end

module Make_without_tests (T : Indexable_without_tests) =
  Make_gen_without_tests (struct
    type 'a elt = T.elt
    type 'a t   = T.t
    include (T : Indexable_without_tests with type elt := T.elt with type t := T.t)
  end)

module Make (T : Indexable) =
  Make_gen (struct
    type 'a elt = T.elt
    type 'a t   = T.t
    include (T : Indexable with type elt := T.elt with type t := T.t)
  end)

module Make1_without_tests (T : Indexable1_without_tests) =
  Make_gen_without_tests (struct
    type 'a elt = 'a
    type 'a t = 'a T.t
    let get = T.get
    let length = T.length
  end)

module Make1 (T : Indexable1) =
  Make_gen (struct
    type 'a elt = 'a
    type 'a t   = 'a T.t

    let get    = T.get
    let length = T.length

    module For_test = struct
      include T.For_test

      let small = false
      let big   = true
    end
  end)
