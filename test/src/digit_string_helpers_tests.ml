open Core_kernel
open Core_kernel_private.Digit_string_helpers

let%test_unit _ =
  for i = 0 to 9999 do
    let s = String.make 4 ' ' in
    blit_string_of_int_4_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%04d" i) s
  done
;;

let%test_unit _ =
  for i = 0 to 99 do
    let s = String.make 2 ' ' in
    blit_string_of_int_2_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%02d" i) s
  done
;;

let%test_unit _ =
  for i = 0 to 999 do
    let s = String.make 3 ' ' in
    blit_string_of_int_3_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%03d" i) s
  done
;;

