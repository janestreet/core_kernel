open! Core

let n = 1_000_000
let times = 10

(** Benchmark the core Hashtbl *)
let test_core () =
  let module Identity_table =
    Hashtbl.Make (struct
      include Int

      let hash x = x
    end)
  in
  let tbl = Identity_table.create ~size:(2 * n) () in
  for i = 0 to n - 1 do
    Hashtbl.set tbl ~key:i ~data:i
  done;
  let start = Time_float.now () in
  let first_n = ref 0 in
  for _ = 0 to times - 1 do
    let loop_end = !first_n + n - 1 in
    for i = !first_n to loop_end do
      let x = n + i in
      Hashtbl.set tbl ~key:x ~data:x;
      Hashtbl.remove tbl i
    done;
    first_n := !first_n + n
  done;
  let elapsed = Time_float.diff (Time_float.now ()) start in
  printf "Core hashtbl took %6f\n" (Time_float.Span.to_sec elapsed /. Float.of_int times);
  printf "Size: %d\n%!" (Hashtbl.length tbl);
  for i = !first_n to !first_n + n - 1 do
    assert (Hashtbl.find_exn tbl i = i)
  done
;;

(** Benchmark Pooled_hashtbl, a linked chain hashtbl backed by a Zero.Obj_array pool *)
let test_zero () =
  let module Identity_table =
    Pooled_hashtbl.Make (struct
      include Int

      let hash x = x
    end)
  in
  let tbl = Identity_table.create ~size:(2 * n) () in
  for i = 0 to n - 1 do
    Pooled_hashtbl.set tbl ~key:i ~data:i
  done;
  let start = Time_float.now () in
  let first_n = ref 0 in
  for _ = 0 to times - 1 do
    let loop_end = !first_n + n - 1 in
    for i = !first_n to loop_end do
      let x = n + i in
      Pooled_hashtbl.set tbl ~key:x ~data:x;
      Pooled_hashtbl.remove tbl i
    done;
    first_n := !first_n + n
  done;
  let elapsed = Time_float.diff (Time_float.now ()) start in
  printf
    "Zero pooled hashtbl took %6f\n"
    (Time_float.Span.to_sec elapsed /. Float.of_int times);
  printf "Size: %d\n%!" (Pooled_hashtbl.length tbl);
  for i = !first_n to !first_n + n - 1 do
    assert (Pooled_hashtbl.find_exn tbl i = i)
  done
;;

let () =
  for _i = 0 to 3 do
    test_core ();
    test_zero ()
  done
;;
