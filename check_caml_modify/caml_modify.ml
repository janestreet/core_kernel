external count : unit -> int = "check_caml_modify_count" "noalloc"
external reset : unit -> unit = "check_caml_modify_reset" "noalloc"

TEST_UNIT =
  let x = Array.create (32 * 1024) [Random.int 10] in
  let v = [Random.int 10] in
  let n = count () in
  x.(0) <- v;
  assert (count () = n + 1);
  let x = Array.create (32 * 1024) 0 in
  let n = count () in
  x.(0) <- 2;
  assert (count () = n);
;;
