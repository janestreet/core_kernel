open! Core_kernel
open  Expect_test_helpers_kernel

let utc date_string ofday_string =
  Time.of_date_ofday
    (Date.of_string date_string)
    (Time.Ofday.of_string ofday_string)
    ~zone:Time.Zone.utc

let examples = [
  Time.epoch;
  utc "2001-01-01" "00:00:00";
  utc "2013-10-07" "09:30:00";
  utc "2017-07-28" "11:57:00.000123";
]

let%expect_test "Time.Stable.With_utc_sexp.V2" =
  print_and_check_stable_type [%here] (module Time.Stable.With_utc_sexp.V2)
    examples;
  [%expect {|
    (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
    ((sexp (1970-01-01 00:00:00.000000Z))
     (bin_io "\000\000\000\000\000\000\000\000"))
    ((sexp (2001-01-01 00:00:00.000000Z)) (bin_io "\000\000\000@\228'\205A"))
    ((sexp (2013-10-07 09:30:00.000000Z))
     (bin_io "\000\000\000\198\159\148\212A"))
    ((sexp (2017-07-28 11:57:00.000123Z)) (bin_io "\004\002\000\163\201^\214A")) |}];
;;
