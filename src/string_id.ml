open! Import
open Std_internal

include String_id_intf

module Make_with_validate_without_pretty_printer (M : sig
    val module_name : string
    val validate : string -> unit Or_error.t
  end) () = struct

  module Stable = struct
    module V1 = struct
      module T = struct
        type t = string
        [@@deriving sexp, compare, hash]

        let check_for_whitespace =
          let invalid s reason =
            Error (sprintf "'%s' is not a valid %s because %s" s M.module_name reason)
          in
          fun s ->
            let len = String.length s in
            if Int.(=) len 0
            then invalid s "it is empty"
            else if Char.is_whitespace s.[0] || Char.is_whitespace s.[len-1]
            then invalid s "it has whitespace on the edge"
            else Ok ()
        ;;

        let validate s =
          Result.map_error (M.validate s) ~f:Error.to_string_mach
        ;;

        let check s =
          match check_for_whitespace s with
          | Ok ()       -> validate s
          | Error error -> Error error
        ;;

        let to_string = Fn.id
        let pp = String.pp

        let of_string s =
          match check s with
          | Ok ()     -> s
          | Error err -> invalid_arg err
        ;;

        let t_of_sexp sexp =
          let s = String.Stable.V1.t_of_sexp sexp in
          match check s with
          | Ok ()     -> s
          | Error err -> of_sexp_error err sexp
        ;;

        include Binable.Of_binable (String)
            (struct
              type nonrec t = t
              let to_binable = Fn.id
              let of_binable = of_string
            end)
      end

      module T_with_comparator = struct
        include T
        include Comparator.Stable.V1.Make (T)
      end

      include T_with_comparator

      include Comparable.Stable.V1.Make          (T_with_comparator)
      include Stable_containers.Hashable.V1.Make (T_with_comparator)

    end
  end

  module Stable_latest = Stable.V1

  include Stable_latest.T_with_comparator
  include Comparable.Make_binable_using_comparator (Stable_latest.T_with_comparator)
  include Hashable.Make_binable                    (Stable_latest.T_with_comparator)
end

module Unit_tests_util = struct
  let to_bin_str t ~bin_writer_t ~bin_size_t =
    let write = bin_writer_t.Bin_prot.Type_class.write in
    let size = bin_size_t t in
    let buf = Bigstring.create size in
    let len = write buf ~pos:0 t in
    assert (len = size);
    Bigstring.to_string buf
  ;;

  let string_to_bin_str t =
    let bin_writer_t = String.bin_writer_t in
    let bin_size_t = String.bin_size_t in
    to_bin_str t ~bin_writer_t ~bin_size_t
  ;;
end

let%test_module "Make_with_validate_without_pretty_printer" =
  (module struct

    (* Even integers are the only valid identifiers *)
    module Even_int_id = Make_with_validate_without_pretty_printer (struct
        let module_name = "Even_int_id"

        let validate s =
          if Int.of_string s % 2 <> 0
          then Or_error.error_s [%message "Not a valid Even_int_id" ~_:(s : string)]
          else Ok ()
        ;;
      end) ()

    let even_int_id_to_bin_str t =
      let bin_writer_t = Even_int_id.bin_writer_t in
      let bin_size_t = Even_int_id.bin_size_t in
      Unit_tests_util.to_bin_str t ~bin_writer_t ~bin_size_t
    ;;

    let even_int_id_of_bin_str str =
      Even_int_id.bin_reader_t.Bin_prot.Type_class.read
        (Bigstring.of_string str) ~pos_ref:(ref 0)
    ;;

    let%test_unit "string roundtrip" =
      [%test_result : string]
        (Even_int_id.of_string "14" |> Even_int_id.to_string)
        ~expect:"14"
    ;;

    let%test_unit "sexp roundtrip" =
      [%test_result : string]
        (Even_int_id.t_of_sexp (Sexp.of_string "14")
         |> Even_int_id.sexp_of_t |> Sexp.to_string)
        ~expect:"14"
    ;;

    let%test_unit "bin_prot roundrip" =
      [%test_result : string]
        (Even_int_id.of_string "14"
         |> even_int_id_to_bin_str
         |> even_int_id_of_bin_str
         |> Even_int_id.to_string)
        ~expect:"14"
    ;;

    let print_error or_error =
      match or_error with
      | Ok _        -> printf "<OK>"
      | Error error -> Error.to_string_hum error |> printf "%s"
    ;;

    let%expect_test "of string failure" =
      Or_error.try_with (fun () -> Even_int_id.of_string "15")
      |> print_error;
      [%expect {|
        (Invalid_argument "(\"Not a valid Even_int_id\"15)") |}]
    ;;

    let%expect_test "of sexp failure" =
      Or_error.try_with (fun () -> Even_int_id.t_of_sexp (Sexp.of_string "15"))
      |> print_error;
      [%expect {|
        (Sexplib.Conv.Of_sexp_error (Failure "(\"Not a valid Even_int_id\"15)") 15) |}]
    ;;

    let%expect_test "set of sexp failure" =
      Or_error.try_with (fun () -> Even_int_id.Set.t_of_sexp (Sexp.of_string "(15)"))
      |> print_error;
      [%expect {|
        (Sexplib.Conv.Of_sexp_error (Failure "(\"Not a valid Even_int_id\"15)") 15) |}]
    ;;

    let%expect_test "of bin prot failure" =
      Or_error.try_with (fun () ->
        even_int_id_of_bin_str (Unit_tests_util.string_to_bin_str "15"))
      |> print_error;
      [%expect {|
        (Invalid_argument "(\"Not a valid Even_int_id\"15)") |}]
    ;;
  end)

module Make_without_pretty_printer (M : sig val module_name : string end) () = struct
  include Make_with_validate_without_pretty_printer (struct
      let module_name = M.module_name
      let validate = Fn.const (Ok ())
    end) ()
end

let%test_module _ =
  (module struct
    module M =
      Make_without_pretty_printer (struct
        let module_name = "test"
      end) ()

    let%test_unit "string roundtrip" =
      [%test_result : string]
        (M.of_string "FOOBAR" |> M.to_string)
        ~expect:"FOOBAR"
    ;;

    let%test_unit "sexp roundtrip" =
      [%test_result : string]
        (M.t_of_sexp (Sexp.of_string "FOOBAR")
         |> M.sexp_of_t |> Sexp.to_string)
        ~expect:"FOOBAR"
    ;;

    let string_id_to_bin_str t =
      let bin_writer_t = M.bin_writer_t in
      let bin_size_t = M.bin_size_t in
      Unit_tests_util.to_bin_str t ~bin_writer_t ~bin_size_t
    ;;

    let string_id_of_bin_str str =
      M.bin_reader_t.Bin_prot.Type_class.read
        (Bigstring.of_string str) ~pos_ref:(ref 0)
    ;;

    let%test_unit "bin_prot roundrip" =
      [%test_result : string]
        (M.of_string "FOOBAR"
         |> string_id_to_bin_str
         |> string_id_of_bin_str
         |> M.to_string)
        ~expect:"FOOBAR"
    ;;

    let%test_unit "whitespace inside is OK" =
      [%test_result : string]
        (M.of_string "FOO  BAR" |> M.to_string)
        ~expect:"FOO  BAR"
    ;;

    let print_error or_error =
      match or_error with
      | Ok _        -> printf "<OK>"
      | Error error -> Error.to_string_hum error |> printf "%s"
    ;;

    let%expect_test "of string failure - empty string" =
      Or_error.try_with (fun () -> M.of_string "")
      |> print_error;
      [%expect {|
        (Invalid_argument "'' is not a valid test because it is empty") |}]
    ;;

    let%expect_test "of string failure - whitespace after" =
      Or_error.try_with (fun () -> M.of_string "FOOBAR ")
      |> print_error;
      [%expect {|
        (Invalid_argument
         "'FOOBAR ' is not a valid test because it has whitespace on the edge") |}]
    ;;

    let%expect_test "of string failure - whitespace before" =
      Or_error.try_with (fun () -> M.of_string " FOOBAR")
      |> print_error;
      [%expect {|
        (Invalid_argument
         "' FOOBAR' is not a valid test because it has whitespace on the edge") |}]
    ;;

    let%expect_test "of sexp failure" =
      Or_error.try_with (fun () -> M.t_of_sexp (Sexp.of_string "\"FOOBAR \""))
      |> print_error;
      [%expect {|
        (Sexplib.Conv.Of_sexp_error
         (Failure
          "'FOOBAR ' is not a valid test because it has whitespace on the edge")
         "FOOBAR ") |}]
    ;;

    let%expect_test "set of sexp failure" =
      Or_error.try_with (fun () -> M.Set.t_of_sexp (Sexp.of_string "(\"FOOBAR \")"))
      |> print_error;
      [%expect {|
        (Sexplib.Conv.Of_sexp_error
         (Failure
          "'FOOBAR ' is not a valid test because it has whitespace on the edge")
         "FOOBAR ") |}]
    ;;

    let%expect_test "of bin prot failure" =
      Or_error.try_with (fun () ->
        string_id_of_bin_str (Unit_tests_util.string_to_bin_str "FOOBAR "))
      |> print_error;
      [%expect {|
        (Invalid_argument
         "'FOOBAR ' is not a valid test because it has whitespace on the edge") |}]
    ;;

    let%test_unit "String_id's of_string shouldn't allocate on success" =
      let initial_words = Gc.minor_words () in
      ignore (M.of_string "FOOBAR");
      let allocated = (Gc.minor_words ()) - initial_words in
      [%test_result: int] allocated ~expect:0
    ;;
  end)

module Make_with_validate (M : sig
    val module_name : string
    val validate : string -> unit Or_error.t
  end) () = struct

  include Make_with_validate_without_pretty_printer(M) ()

  include Pretty_printer.Register (struct
      type nonrec t = t
      let module_name = M.module_name
      let to_string = to_string
    end)
end

module Make (M : sig val module_name : string end) () = struct
  include Make_without_pretty_printer(M) ()

  include Pretty_printer.Register (struct
      type nonrec t = t
      let module_name = M.module_name
      let to_string = to_string
    end)
end

include Make (struct let module_name = "Core_kernel.String_id" end) ()
