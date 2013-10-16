open Std

TEST_MODULE "test_binary_searchable" = struct
  TEST = Array.binary_search ~compare [||]  7           = None
  TEST = Array.binary_search ~compare [| 7 |]  7        = Some 0
  TEST = Array.binary_search ~compare [| 7 |]  8        = None
  TEST = Array.binary_search ~compare [| 1 ; 2 |]  1    = Some 0
  TEST = Array.binary_search ~compare [| 1 ; 2 |]  2    = Some 1
  TEST = Array.binary_search ~compare [| 1 ; 2 |]  0    = None
  TEST = Array.binary_search ~compare [| 1 ; 2 |]  3    = None
  TEST = Array.binary_search ~compare [| 1 ; 2; 3 |]  2 = Some 1
  TEST = Array.binary_search ~compare [| 1 ; 2; 3 |]  2 = Some 1

  let create_test_case rand =
    let arr = Array.create ~len:1_000 0 in
    Array.iteri arr ~f:(fun i _ -> arr.(i) <- rand ());
    Array.sort arr ~cmp:compare;
    arr
  ;;

  let one ()   = 1
  let small () = match Random.int 10 with 6 -> 5 | n -> n
  let large () = match Random.int 100_000 with 30_000 -> 30_001 | n -> n

  TEST = Option.is_some (Array.binary_search (create_test_case one) ~compare 1)

  TEST =
    let arr = create_test_case small in
    let expect = arr.(250) in
    match Array.binary_search arr expect ~compare with
    | None -> false
    | Some v -> arr.(v) = expect

  TEST =
    let arr = create_test_case small in
    Array.binary_search arr 6 ~compare = None

  TEST =
    let arr = create_test_case large in
    let expect = arr.(250) in
    match Array.binary_search arr expect ~compare with
    | None -> false
    | Some v -> arr.(v) = expect

  TEST =
    let arr = create_test_case small in
    Array.binary_search arr 30_000 ~compare = None

  TEST_UNIT =
    for length = 0 to 5 do
      let arr = Array.init length ~f:Fn.id in
      for search_key = -1 to length do
        for pos = -1 to length do
          for len = -1 to length + 1 do
            try
              let should_raise =
                does_raise (fun () ->
                  Ordered_collection_common.check_pos_len_exn ~pos ~len ~length)
              in
              let result =
                Or_error.try_with (fun () ->
                  Array.binary_search arr search_key ~pos ~len ~compare)
              in
              match should_raise, result with
              | true , Error _   -> ()
              | true , Ok _      -> failwith "expected it to raise but it didn't"
              | false, Error _   -> failwith "expected it to not raise, but it raised"
              | false, Ok result ->
                let search_key_is_in_slice =
                  pos <= search_key && search_key < pos + len
                in
                let expect =
                  if search_key_is_in_slice
                  then Some search_key
                  else None
                in
                if result <> expect then
                  failwiths "mismatch" (`result result, `expect expect)
                    <:sexp_of< [ `result of int option ] * [ `expect of int option ] >>
            with exn ->
              failwiths "binary_search bug"
                (exn, `length length, `search_key search_key, `pos pos, `len len)
                <:sexp_of< exn * [ `length of int ] * [ `search_key of int ]
                          * [ `pos of int ] * [ `len of int ] >>
          done;
        done;
      done;
    done;
  ;;
end

