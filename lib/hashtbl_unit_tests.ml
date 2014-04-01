open Std_internal

module Make (Hashtbl : Core_hashtbl_intf.Hashtbl) = struct

  let test_data = [("a",1);("b",2);("c",3)]

  let test_hash = begin
    let h = Hashtbl.Poly.create () ~size:10 in
    List.iter test_data ~f:(fun (k,v) ->
      Hashtbl.replace h ~key:k ~data:v
    );
    h
  end

  (* This is a very strong notion of equality on hash tables *)
  let equal t t' equal_data =
    let subtable t t' =
      try
        List.for_all (Hashtbl.keys t) ~f:(fun key ->
          equal_data (Hashtbl.find_exn t key) (Hashtbl.find_exn t' key))
      with
      | Invalid_argument _ -> false
    in
    subtable t t' && subtable t' t

  TEST "find" =
    let found = Hashtbl.find test_hash "a" in
    let not_found = Hashtbl.find test_hash "A" in
    Hashtbl.invariant test_hash;
    match found,not_found with
    | Some _, None -> true
    | _ -> false
  ;;

  TEST_UNIT "add" =
    let our_hash = Hashtbl.copy test_hash in
    let duplicate = Hashtbl.add our_hash ~key:"a" ~data:4 in
    let no_duplicate = Hashtbl.add our_hash ~key:"d" ~data:5 in
    assert (Hashtbl.find our_hash "a" = Some 1);
    assert (Hashtbl.find our_hash "d" = Some 5);
    Hashtbl.invariant our_hash;
    assert (match duplicate, no_duplicate with
            | `Duplicate, `Ok -> true
            | _ -> false)
  ;;

  TEST "iter_vals" =
    let predicted = List.sort ~cmp:Int.descending (
      List.map test_data ~f:(fun (_,v) -> v))
    in
    let found =
      let found = ref [] in
      Hashtbl.iter_vals test_hash ~f:(fun v -> found := v :: !found);
      !found
      |> List.sort ~cmp:Int.descending
    in
    List.equal ~equal:Int.equal predicted found
  ;;

  TEST_MODULE "of_alist" = struct

    TEST "size" =
      let predicted = List.length test_data in
      let found = Hashtbl.length (Hashtbl.Poly.of_alist_exn test_data) in
      predicted = found
    ;;

    TEST "right keys" =
      let predicted = List.map test_data ~f:(fun (k,_) -> k) in
      let found = Hashtbl.keys (Hashtbl.Poly.of_alist_exn test_data) in
      let sp = List.sort ~cmp:Poly.ascending predicted in
      let sf = List.sort ~cmp:Poly.ascending found in
      sp = sf
    ;;
  end

  TEST_MODULE "of_alist_or_error" = struct

    TEST "unique" =
      Result.is_ok (Hashtbl.Poly.of_alist_or_error test_data)

    TEST "duplicate" =
      Result.is_error (Hashtbl.Poly.of_alist_or_error (test_data @ test_data))

  end

  TEST "size and right keys" =
    let predicted = List.map test_data ~f:(fun (k,_) -> k) in
    let found = Hashtbl.keys test_hash in
    let sp = List.sort ~cmp:Poly.ascending predicted in
    let sf = List.sort ~cmp:Poly.ascending found in
    sp = sf
  ;;

  TEST "size and right data" =
    let predicted = List.map test_data ~f:(fun (_,v) -> v) in
    let found = Hashtbl.data test_hash in
    let sp = List.sort ~cmp:Poly.ascending predicted in
    let sf = List.sort ~cmp:Poly.ascending found in
    sp = sf
  ;;

  TEST "map" =
    let add1 x = x + 1 in
    let predicted_data =
      List.sort ~cmp:Poly.ascending
        (List.map test_data ~f:(fun (k,v) -> (k,add1 v)))
    in
    let found_alist =
      Hashtbl.map test_hash ~f:add1
      |> Hashtbl.to_alist
      |> List.sort ~cmp:Poly.ascending
    in
    List.equal predicted_data found_alist ~equal:Poly.equal
  ;;

  TEST_UNIT "filter_map" =
    let f x = Some x in
    let result = Hashtbl.filter_map test_hash ~f in
    assert (equal test_hash result Int.(=));
    let is_even x = x mod 2 = 0 in
    let add1_to_even x = if is_even x then Some (x + 1) else None in
    let predicted_data = List.filter_map test_data ~f:(fun (k,v) ->
      if is_even v then Some (k, v+1) else None)
    in
    let found = Hashtbl.filter_map test_hash ~f:add1_to_even in
    let found_alist =
      List.sort ~cmp:Poly.ascending (Hashtbl.to_alist found)
    in
    assert (List.equal predicted_data found_alist ~equal:Poly.equal);
  ;;

  TEST_UNIT "insert-find-remove" =
    let t = Hashtbl.Poly.create () ~size:1 in
    let inserted = ref [] in
    Random.self_init ();
    let verify_inserted t =
      let missing =
        List.fold !inserted ~init:[] ~f:(fun acc (key, data) ->
          match Hashtbl.find t key with
          | None -> `Missing key :: acc
          | Some d ->
            if data = d then acc
            else `Wrong_data (key, data) :: acc)
      in
      match missing with
      | [] -> ()
      | l ->
        failwiths "some inserts are missing"
          l
          <:sexp_of< [`Missing of int | `Wrong_data of int * int ] list>>
    in
    let rec loop i t =
      if i < 2000 then begin
        let k = Random.int 10_000 in
        inserted := List.Assoc.add (List.Assoc.remove !inserted k) k i;
        Hashtbl.replace t ~key:k ~data:i;
        Hashtbl.invariant t;
        verify_inserted t;
        loop (i + 1) t
      end
    in
    loop 0 t;
    List.iter !inserted ~f:(fun (x, _) ->
      Hashtbl.remove t x;
      Hashtbl.invariant t;
      begin match Hashtbl.find t x with
      | None -> ()
      | Some _ -> failwith (sprintf "present after removal: %d" x)
      end;
      inserted := List.Assoc.remove !inserted x;
      verify_inserted t)
  ;;

  TEST_UNIT "clear" =
    let t = Hashtbl.Poly.create () ~size:1 in
    let l = List.range 0 100 in
    let verify_present l = List.for_all l ~f:(Hashtbl.mem t) in
    let verify_not_present l =
      List.for_all l ~f:(fun i -> not (Hashtbl.mem t i))
    in
    List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
    List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
    assert (Hashtbl.length t = 100);
    assert (verify_present l);
    Hashtbl.clear t;
    Hashtbl.invariant t;
    assert (Hashtbl.length t = 0);
    assert (verify_not_present l);
    let l = List.take l 42 in
    List.iter l ~f:(fun i -> Hashtbl.replace t ~key:i ~data:(i * i));
    assert (Hashtbl.length t = 42);
    assert (verify_present l);
    Hashtbl.invariant t;
  ;;

  TEST_UNIT "mem" =
    let t = Hashtbl.Poly.create () ~size:1 in
    Hashtbl.invariant t;
    assert (not (Hashtbl.mem t "Fred"));
    Hashtbl.invariant t;
    Hashtbl.replace t ~key:"Fred" ~data:"Wilma";
    Hashtbl.invariant t;
    assert (Hashtbl.mem t "Fred");
    Hashtbl.invariant t;
    Hashtbl.remove t "Fred";
    Hashtbl.invariant t;
    assert (not (Hashtbl.mem t "Fred"));
    Hashtbl.invariant t;
  ;;

  TEST_UNIT "exists" =
    let t = Hashtbl.Poly.create () in
    assert (not (Hashtbl.exists t ~f:(fun _ -> failwith "can't be called")));
    Hashtbl.set t ~key:7 ~data:3;
    assert (not (Hashtbl.exists t ~f:(Int.equal 4)));
    Hashtbl.set t ~key:8 ~data:4;
    assert (Hashtbl.exists t ~f:(Int.equal 4));
    Hashtbl.set t ~key:9 ~data:5;
    assert (Hashtbl.existsi t ~f:(fun ~key ~data -> key + data = 14))

  TEST_UNIT "for_all" =
    let t = Hashtbl.Poly.create () in
    assert (Hashtbl.for_all t ~f:(fun _ -> failwith "can't be called"));
    Hashtbl.set t ~key:7 ~data:3;
    assert (Hashtbl.for_all t ~f:(fun x -> Int.equal x 3));
    Hashtbl.set t ~key:8 ~data:4;
    assert (not (Hashtbl.for_all t ~f:(fun x -> Int.equal x 3)));
    Hashtbl.set t ~key:9 ~data:5;
    assert (Hashtbl.for_alli t ~f:(fun ~key ~data -> key - 4 = data));
end

TEST_MODULE = Make(Hashtbl)
