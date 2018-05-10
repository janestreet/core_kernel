open! Core_kernel
open! Import
open! Byte_units

let bytes_per_word = Private.bytes_per_word
let kbyte = 1024.

let%expect_test "Byte_units.to_string_hum" [@tags "64-bits-only"] =
  print_string (Byte_units.to_string_hum (Byte_units.create `Bytes 1000.));
  [%expect {| 1000b |}];
  print_string (Byte_units.to_string_hum (Byte_units.create `Bytes 1500.));
  [%expect {| 1.46484k |}];
  print_string (Byte_units.to_string_hum ~measure:`Gigabytes (Byte_units.create `Bytes 1000.));
  [%expect {| 9.31323e-07g |}];
  print_string (Byte_units.to_string_hum ~measure:`Words (Byte_units.create `Bytes 1000.));
  [%expect {| 125w |}];
;;

let%test_module "{of,to}_string" =
  (module struct

    let f measure input expected_output =
      let observed_output =
        match measure with
        | `Specific measure ->
          to_string_hum ~measure (of_string input)
        | `Largest ->
          to_string_hum (of_string input)
      in

      let result = String.equal expected_output observed_output in
      if not result then begin
        let measure =
          [%sexp_of: [ `Specific of Measure.t | `Largest ]] measure
          |> Sexp.to_string
        in
        eprintf "\n(%s) %s -> %s != %s\n%!" measure input expected_output observed_output
      end;
      result

    let%test _ = f `Largest "3b" "3b"
    let%test _ = f `Largest "3w" (sprintf "%gb" (3.0 *. bytes_per_word))
    let%test _ = f `Largest "3k" "3k"
    let%test _ = f `Largest "3m" "3m"
    let%test _ = f `Largest "3g" "3g"

    let%test _ = f (`Specific `Bytes)     "3k" "3072b"
    let%test _ = f (`Specific `Kilobytes) "3k" "3k"
    let%test _ = f (`Specific `Megabytes) "3k" "0.00292969m"
    let%test _ = f (`Specific `Gigabytes) "3k" "2.86102e-06g"
    let%test _ = f (`Specific `Words)     "3k" (sprintf "%gw" ((3.0 *. kbyte) /. bytes_per_word))

  end)
