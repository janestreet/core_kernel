open Std_internal
open Import

TEST_MODULE = struct
  module Hashtbl = Pooled_hashtbl.Make(struct
    include Int
    let hash x = x
  end)

  TEST_UNIT "growing test/copy test" =
    let n = 1_000_000 in
    let tbl = Hashtbl.create ~size:16 () in
    for i = 0 to n - 1 do
      Hashtbl.replace tbl ~key:i ~data:i;
    done;
    let first_n = ref 0 in
    for _z = 1 to 2 do
      let loop_end = !first_n + n - 1 in
      for i = !first_n to loop_end do
        let x = n + i in
        Hashtbl.replace tbl ~key:x ~data:x;
        Hashtbl.remove tbl i;
      done;
      first_n := !first_n + n
    done;
    for i = !first_n to !first_n + n - 1 do
      assert ( (Hashtbl.find_exn tbl i) = i )
    done;

    let tbl_copy = Hashtbl.copy tbl in
    assert ( (Hashtbl.length tbl) = (Hashtbl.length tbl_copy) );
    for i = !first_n to !first_n + n - 1 do
      assert ( (Hashtbl.find_exn tbl_copy i) = i )
    done;

    (* assert no sharing *)
    Hashtbl.clear tbl;
    assert ( (Hashtbl.length tbl) = 0 );
    for i = !first_n to !first_n + n - 1 do
      assert ( not (Hashtbl.mem tbl i) );
      assert ( (Hashtbl.find_exn tbl_copy i) = i )
    done;
  ;;

  TEST_UNIT "adding elements when growth_allowed=false" =
    let n = 1_000 in
    let tbl = Hashtbl.create ~size:16 ~growth_allowed:false () in
    for i = 0 to n - 1 do
      Hashtbl.set tbl ~key:i ~data:i;
    done;
  ;;

  (* Test for past bugs with very small tables and collisions *)
  TEST_UNIT "tiny map, colliding keys" =
    let tbl = Hashtbl.create ~size:3 () in
    Hashtbl.set tbl ~key:3146414644118347 ~data:0;
    assert((Hashtbl.find_exn tbl 3146414644118347) = 0);
    Hashtbl.set tbl ~key:5455654276678475 ~data:1;
    assert((Hashtbl.find_exn tbl 5455654276678475) = 1);
    Hashtbl.set tbl ~key:151277324321611 ~data:2;
    assert((Hashtbl.find_exn tbl 151277324321611) = 2);
  ;;

  TEST_UNIT "collision test" =
    (* Under identity hashing, this creates a collision and chain on bucket zero... *)
    let t = Hashtbl.create ~size:20 () in
    let s1 = 128162763636736 in
    let s2 = 2250965516288 in
    Hashtbl.add_exn t ~key:s1 ~data:0;
    assert ((Hashtbl.find_exn t s1) = 0);
    Hashtbl.add_exn t ~key:s2 ~data:1;
    assert ((Hashtbl.find_exn t s1) = 0);
    assert ((Hashtbl.find_exn t s2) = 1);
  ;;

  TEST_UNIT "simple iter/fold" =
    let tbl = Hashtbl.create ~size:64 () in
    let n = 100 in
    for i=1 to 100 do
      Hashtbl.set tbl ~key:i ~data:i
    done;
    let isum = ref 0 in
    Hashtbl.iter tbl ~f:(fun ~key ~data -> assert (key = data); isum := !isum + key);
    let fsum =
      Hashtbl.fold tbl ~init:0 ~f:(fun ~key ~data acc ->
        assert (key = data);
        acc + key)
    in
    assert (n = Hashtbl.length tbl);
    assert (fsum = !isum);
    let expected_sum = (n * (n+1)) / 2 in
    assert (fsum = expected_sum);
  ;;
end

TEST_MODULE "unit tests from core" = Hashtbl_unit_tests.Make (Pooled_hashtbl)
