open! Core_kernel

let%test_unit "String.exists doesn't allocate" =
  let initial_words = Gc.minor_words () in
  assert (String.exists "FOOBAR" ~f:Char.is_uppercase);
  assert (not (String.exists "FOOBAR" ~f:Char.is_lowercase));
  let allocated = (Gc.minor_words ()) - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "String.for_all doesn't allocate" =
  let initial_words = Gc.minor_words () in
  assert (String.for_all "FOOBAR" ~f:Char.is_uppercase);
  assert (not (String.for_all "FOOBAR" ~f:Char.is_lowercase));
  let allocated = (Gc.minor_words ()) - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "String.is_suffix doesn't allocate" =
  let initial_words = Gc.minor_words () in
  assert (String.is_suffix "FOOBAR" ~suffix:"BAR");
  assert (not (String.is_suffix "FOOBAR" ~suffix:"BUZ"));
  let allocated = (Gc.minor_words ()) - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "String.is_prefix doesn't allocate" =
  let initial_words = Gc.minor_words () in
  assert (String.is_prefix "FOOBAR" ~prefix:"FOO");
  assert (not (String.is_prefix "FOOBAR" ~prefix:"FUZ"));
  let allocated = (Gc.minor_words ()) - initial_words in
  [%test_result: int] allocated ~expect:0
;;
