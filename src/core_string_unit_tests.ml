(* These tests are here to avoid a circular dependency between Core_string and Core_gc. *)
open Std_internal

let%test_unit "Core_string.exists doesn't allocate" =
  let initial_words = Core_gc.minor_words () in
  assert (Core_string.exists "FOOBAR" ~f:Char.is_uppercase);
  assert (not (Core_string.exists "FOOBAR" ~f:Char.is_lowercase));
  let allocated = (Core_gc.minor_words ()) - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "Core_string.for_all doesn't allocate" =
  let initial_words = Core_gc.minor_words () in
  assert (Core_string.for_all "FOOBAR" ~f:Char.is_uppercase);
  assert (not (Core_string.for_all "FOOBAR" ~f:Char.is_lowercase));
  let allocated = (Core_gc.minor_words ()) - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "Core_string.is_suffix doesn't allocate" =
  let initial_words = Core_gc.minor_words () in
  assert (Core_string.is_suffix "FOOBAR" ~suffix:"BAR");
  assert (not (Core_string.is_suffix "FOOBAR" ~suffix:"BUZ"));
  let allocated = (Core_gc.minor_words ()) - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "Core_string.is_prefix doesn't allocate" =
  let initial_words = Core_gc.minor_words () in
  assert (Core_string.is_prefix "FOOBAR" ~prefix:"FOO");
  assert (not (Core_string.is_prefix "FOOBAR" ~prefix:"FUZ"));
  let allocated = (Core_gc.minor_words ()) - initial_words in
  [%test_result: int] allocated ~expect:0
;;
