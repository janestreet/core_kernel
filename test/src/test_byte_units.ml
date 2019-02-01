open! Core_kernel
open! Import
open! Byte_units

let bytes_per_word = Float.of_int (Word_size.num_bits Word_size.word_size / 8)
let kbyte = 1024.

let%expect_test ("Byte_units.to_string_hum"[@tags "64-bits-only"]) =
  print_string (Byte_units.to_string_hum (Byte_units.of_bytes_int 1000));
  [%expect {| 1000b |}];
  print_string (Byte_units.to_string_hum (Byte_units.of_bytes_int 1500));
  [%expect {| 1.46484k |}]
;;

let%test_module "{of,to}_string" =
  (module struct
    let f input expected_output =
      let observed_output = to_string_hum (of_string input) in
      let result = String.equal expected_output observed_output in
      if not result
      then eprintf "\n%s -> %s != %s\n%!" input expected_output observed_output;
      result
    ;;

    let%test _ = f "3b" "3b"
    let%test _ = f "3w" (sprintf "%gb" (3.0 *. bytes_per_word))
    let%test _ = f "3k" "3k"
    let%test _ = f "3m" "3m"
    let%test _ = f "3g" "3g"
  end)
;;
