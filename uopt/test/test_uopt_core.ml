open! Core

let%test_unit "bin-prot roundtrip and binary compatibility with regular options" =
  [%test_result: string]
    [%bin_digest: string Uopt_core.t]
    ~expect:[%bin_digest: string option];
  Quickcheck.iter [%quickcheck.generator: string option] ~f:(fun option ->
    let buf = Bin_prot.Utils.bin_dump ~header:false [%bin_writer: string option] option in
    let pos_ref = ref 0 in
    let t = [%bin_read: string Uopt_core.t] buf ~pos_ref in
    [%test_result: int] !pos_ref ~expect:(Bigstring.length buf);
    [%test_result: Bigstring.t]
      (Bin_prot.Utils.bin_dump ~header:false [%bin_writer: string Uopt_core.t] t)
      ~expect:buf)
;;
