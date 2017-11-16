open Core_kernel
open Core_kernel_private.Digit_string_helpers

let%test_unit _ =
  for i = 0 to 9999 do
    let s = Bytes.make 4 ' ' in
    blit_string_of_int_4_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%04d" i) (Bytes.to_string s)
  done
;;

let%test_unit _ =
  for i = 0 to 99 do
    let s = Bytes.make 2 ' ' in
    blit_string_of_int_2_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%02d" i) (Bytes.to_string s)
  done
;;

let%test_unit _ =
  for i = 0 to 999 do
    let s = Bytes.make 3 ' ' in
    blit_string_of_int_3_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%03d" i) (Bytes.to_string s)
  done
;;

