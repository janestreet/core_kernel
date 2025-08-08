open! Core
open Poly
module IR = Int_repr
open! Iobuf
open Expect_test_helpers_core

let arch_sixtyfour = Sys.word_size_in_bits = 64
let is_error = Result.is_error
let is_ok = Result.is_ok
let ok_exn = Or_error.ok_exn
let try_with = Or_error.try_with

module%test [@name "[Consume]"] _ = struct
  let%test_unit "bin_prot char" =
    let t = of_string "abc" in
    let a = Bin_io.consume Char.bin_reader_t t in
    let b = Bin_io.consume Char.bin_reader_t t in
    [%test_eq: char] a 'a';
    [%test_eq: char] b 'b';
    [%test_eq: string] (to_string t) "c"
  ;;

  let%test_unit "bin_prot int" =
    let ints = [ 0; 1; -1; 12345; -67890; Int.min_value; Int.max_value; 666 ] in
    let buf = Bigstring.create 1000 in
    let _end_pos = List.fold ints ~init:0 ~f:(fun pos i -> Int.bin_write_t buf ~pos i) in
    let t = of_bigstring buf in
    List.iter ints ~f:(fun i -> [%test_eq: int] i (Bin_io.consume Int.bin_reader_t t))
  ;;
end

module%test [@name "[Peek]"] _ = struct
  let%test_unit "bin_prot char" =
    let t = of_string "abc" in
    let a = Bin_io.peek Char.bin_read_t t ~pos:0 in
    let b = Bin_io.peek Char.bin_read_t t ~pos:1 in
    [%test_eq: char] a 'a';
    [%test_eq: char] b 'b';
    [%test_eq: string] (to_string t) "abc"
  ;;

  let%test_unit "bin_prot int" =
    let ints = [ 0; 1; -1; 12345; -67890; Int.min_value; Int.max_value; 666 ] in
    let buf = Bigstring.create 1000 in
    let end_pos = List.fold ints ~init:0 ~f:(fun pos i -> Int.bin_write_t buf ~pos i) in
    let t = of_bigstring buf in
    List.fold ints ~init:0 ~f:(fun pos i ->
      [%test_eq: int] i (Bin_io.peek Int.bin_read_t t ~pos);
      pos + Int.bin_size_t i)
    |> fun end_pos' -> [%test_eq: int] end_pos end_pos'
  ;;
end

module%test [@name "[Poke]"] _ = struct
  let%test_unit _ =
    let t = of_string "abc" in
    Bin_io.poke Char.bin_writer_t t 'd' ~pos:0;
    Bin_io.poke Char.bin_writer_t t 'e' ~pos:1;
    [%test_eq: string] "dec" (to_string t);
    flip_lo t;
    assert (
      try
        Bin_io.poke String.bin_writer_t t "fgh" ~pos:0;
        false
      with
      | _ -> true);
    assert (is_empty t);
    reset t;
    [%test_eq: string] "dec" (to_string t);
    Bin_io.poke Char.bin_writer_t t 'i' ~pos:0;
    [%test_eq: string] "iec" (to_string t)
  ;;
end

module type Accessors_common = Accessors_common
module type Accessors_read = Accessors_read
module type Accessors_write = Accessors_write
module type Consuming_blit = Consuming_blit

type nonrec ('d, 'w) t = ('d, 'w) Hexdump.t [@@deriving sexp_of]
type nonrec seek = seek [@@deriving sexp_of]
type nonrec no_seek = no_seek [@@deriving sexp_of]

module type Bound = Bound

(* see iobuf_view_tests.ml for expect-tests of these *)

let read_only = read_only
let no_seek = no_seek

let strings =
  [ ""; "a"; "hello"; "\000"; "\000\000\000"; "\000hello"; String.make 1000 'x' ]
;;

(* [large_int] creates an int that is not representable on 32-bit systems. *)
let large_int a b c d = (a lsl 48) lor (b lsl 32) lor (c lsl 16) lor d

let iter_examples ~f =
  List.iter strings ~f:(fun string ->
    let len = String.length string in
    for capacity = max 1 len to len + 2 do
      for pos = 0 to 2 do
        if pos + len <= capacity
        then (
          try f (create ~len:capacity) string ~pos with
          | e ->
            raise_s
              [%sexp
                "iter_examples"
                , { string : string; len : int; capacity : int; pos : int }
                , (e : exn)])
      done
    done)
;;

type iter_slices_state =
  { pos : int option
  ; len : int option
  ; pos' : int
  ; len' : int
  ; is_valid : bool
  }
[@@deriving sexp]

let iter_slices n ~f =
  let choices =
    [ None; Some (-1); Some 0; Some 1; Some (n / 3); Some (n - 1); Some n; Some (n + 1) ]
  in
  List.iter choices ~f:(fun pos ->
    List.iter choices ~f:(fun len ->
      let pos' = Option.value pos ~default:0 in
      let len' = Option.value len ~default:(n - pos') in
      let is_valid = pos' >= 0 && pos' <= n && len' >= 0 && len' <= n - pos' in
      try f ?pos ?len ~pos' ~len' ~is_valid () with
      | e ->
        raise_s
          [%sexp
            "iter_slices"
            , ({ pos; len; pos'; len'; is_valid } : iter_slices_state)
            , (e : exn)
            , (Backtrace.Exn.most_recent () : Backtrace.t)]))
;;

let invariant = invariant
let%test_unit _ = invariant ignore ignore (create ~len:(Random.int 100_000 + 1))
let create = create
let capacity = capacity
let length = length
let length_lo = length_lo
let length_hi = length_hi
let is_empty = is_empty

(* [create] with random capacity. *)
module%test _ = struct
  let () =
    let n = Random.int 100_000 + 1 in
    assert (n > 0);
    let t = create ~len:n in
    assert (capacity t = n);
    assert (length t = n);
    assert (not (is_empty t))
  ;;
end

(* [create] with user-supplied capacity. *)
let%test_unit _ =
  let t = create ~len:1 in
  assert (capacity t = 1);
  assert (length t = 1);
  assert (not (is_empty t))
;;

let%test _ = is_empty (create ~len:0)

(* [create] with invalid capacity. *)
let%test_unit _ =
  assert (is_error (try_with (fun () -> ignore (create ~len:(-1) : (_, _) t))))
;;

module%test [@name "lengths"] _ = struct
  let%expect_test "length" =
    let print_lengths t =
      printf "%d + %d + %d = %d\n" (length_lo t) (length t) (length_hi t) (capacity t)
    in
    let t = create ~len:5 in
    print_lengths t;
    [%expect {| 0 + 5 + 0 = 5 |}];
    advance t 1;
    print_lengths t;
    [%expect {| 1 + 4 + 0 = 5 |}];
    flip_lo t;
    print_lengths t;
    [%expect {| 0 + 1 + 4 = 5 |}];
    resize t ~len:3;
    advance t 1;
    print_lengths t;
    [%expect {| 1 + 2 + 2 = 5 |}];
    flip_hi t;
    print_lengths t;
    [%expect {| 3 + 2 + 0 = 5 |}]
  ;;
end

module IR_Int8 = struct
  include IR.Int8

  let globalize _ = failwith "not supported"
  let to_string x = x |> to_base_int |> Int.to_string
  let of_string s = s |> Int.of_string |> of_base_int_exn
end

module IR_Uint8 = struct
  include IR.Uint8

  let globalize _ = failwith "not supported"
  let to_string x = x |> to_base_int |> Int.to_string
  let of_string s = s |> Int.of_string |> of_base_int_exn
end

module IR_Int16 = struct
  include IR.Int16

  let globalize _ = failwith "not supported"
  let to_string x = x |> to_base_int |> Int.to_string
  let of_string s = s |> Int.of_string |> of_base_int_exn
end

module IR_Uint16 = struct
  include IR.Uint16

  let globalize _ = failwith "not supported"
  let to_string x = x |> to_base_int |> Int.to_string
  let of_string s = s |> Int.of_string |> of_base_int_exn
end

module IR_Int32 = struct
  include IR.Int32

  let globalize _ = failwith "not supported"
  let to_string x = x |> to_base_int32 |> Int32.to_string
  let of_string s = s |> Int32.of_string |> of_base_int32
end

module IR_Uint32 = struct
  include IR.Uint32

  let globalize _ = failwith "not supported"
  let to_string x = x |> to_base_int32_trunc |> Int32.to_string
  let of_string s = s |> Int32.of_string |> of_base_int32_trunc
end

module IR_Int64 = struct
  include IR.Int64

  let globalize = globalize_int64
  let to_string = Int64.to_string
  let of_string = Int64.of_string
end

module Accessors (Accessors : sig
    module Consume : module type of Consume
    module Fill : module type of Fill
    module Peek : module type of Peek
    module Poke : module type of Poke

    val is_safe : bool
  end) =
struct
  open Accessors

  (* [window] *)
  let%test_unit _ =
    let t = create ~len:10 in
    let n = length t in
    assert (n = 10);
    Poke.char t ~pos:0 'a';
    Poke.char t ~pos:1 'b';
    (sub_shared t
     |> fun t ->
     assert (length t = n);
     assert (Consume.char t = 'a'));
    assert (Consume.char t = 'a');
    assert (Consume.char t = 'b')
  ;;

  let in_each_window t ~f =
    let n = length t in
    for pos = 0 to n do
      for len = 0 to n - pos do
        sub_shared t ~pos ~len |> f
      done
    done
  ;;

  (* [sub_shared ?pos ?len]  *)
  let%test_unit _ =
    let test t =
      let n = length t in
      let window_length ?pos ?len () = sub_shared t ?pos ?len |> length in
      let window_fails ?pos ?len () =
        is_error (try_with (fun () -> ignore (sub_shared t ?pos ?len : _ t)))
      in
      assert (window_fails ~pos:(-1) ());
      assert (window_fails ~pos:(n + 1) ());
      assert (window_fails ~len:(-1) ());
      assert (window_fails ~len:(n + 1) ());
      assert (window_length () = n);
      for len = 0 to n do
        assert (window_length ~len () = len)
      done;
      for pos = 0 to n do
        assert (window_length ~pos () = n - pos);
        for len = 0 to n - pos do
          assert (window_length ~pos ~len () = len)
        done
      done
    in
    for len = 1 to 5 do
      let t = create ~len in
      test t;
      in_each_window t ~f:test
    done
  ;;

  module%test [@name "duplication tests"] _ = struct
    let equal_in_window t t' =
      assert (length t = length t');
      assert (String.equal (to_string t) (to_string t'));
      assert (not (phys_equal (Expert.buf t) (Expert.buf t')))
    ;;

    type assertion =
      { in_window : string
      ; in_limits : string
      ; buf : string
      }

    let run_scenario dup ~expect_t ~expect_a ~expect_b ~expect_c ~expect_d =
      (* Use a [t] where the full window does not encompass the entire buffer *)
      let t = of_bigstring (Bigstring.of_string "0123456") ~pos:1 ~len:5 in
      (* Test with full window *)
      let a = dup t in
      equal_in_window t a;
      resize t ~len:3;
      advance t 1;
      (* Test with new window *)
      let b = dup t in
      equal_in_window t b;
      Poke.char t ~pos:0 '_';
      (* Modifying t buffer should not affect those copied before *)
      let c = dup t in
      equal_in_window t c;
      (* Modifying duplicate buffer only affects that instance *)
      let d = dup t in
      Poke.char d ~pos:0 'X';
      let check_contents t { in_window; in_limits; buf } =
        assert (String.equal (to_string t) in_window);
        reset t;
        assert (String.equal (to_string t) in_limits);
        assert (String.equal (Bigstring.to_string (Expert.buf t)) buf)
      in
      check_contents t expect_t;
      check_contents a expect_a;
      check_contents b expect_b;
      check_contents c expect_c;
      check_contents d expect_d
    ;;

    (* [copy t] *)
    let%test_unit _ =
      run_scenario
        copy
        ~expect_t:{ in_window = "_3"; in_limits = "1_345"; buf = "01_3456" }
        ~expect_a:{ in_window = "12345"; in_limits = "12345"; buf = "12345" }
        ~expect_b:{ in_window = "23"; in_limits = "23"; buf = "23" }
        ~expect_c:{ in_window = "_3"; in_limits = "_3"; buf = "_3" }
        ~expect_d:{ in_window = "X3"; in_limits = "X3"; buf = "X3" }
    ;;

    (* [clone t] *)
    let%test_unit _ =
      run_scenario
        clone
        ~expect_t:{ in_window = "_3"; in_limits = "1_345"; buf = "01_3456" }
        ~expect_a:{ in_window = "12345"; in_limits = "12345"; buf = "0123456" }
        ~expect_b:{ in_window = "23"; in_limits = "12345"; buf = "0123456" }
        ~expect_c:{ in_window = "_3"; in_limits = "1_345"; buf = "01_3456" }
        ~expect_d:{ in_window = "X3"; in_limits = "1X345"; buf = "01X3456" }
    ;;
  end

  module Lo_bound = struct
    type t = Lo_bound.t [@@deriving compare]

    let window, restore = Lo_bound.(window, restore)

    let%test _ =
      let iobuf = create ~len:2 in
      let snapshot = window iobuf in
      assert (length iobuf = 2);
      advance iobuf 1;
      assert (length iobuf = 1);
      restore snapshot iobuf;
      assert (length iobuf = 2);
      (* the same snapshot can be reused *)
      advance iobuf 1;
      assert (length iobuf = 1);
      restore snapshot iobuf;
      assert (length iobuf = 2);
      (* but can fail in combination with narrow *)
      advance iobuf 1;
      narrow iobuf;
      assert (capacity iobuf = 1);
      assert (length iobuf = 1);
      is_error (try_with (fun () -> restore snapshot iobuf))
    ;;

    let limit = Lo_bound.limit

    let%test _ =
      let buf = of_string "123abcDEF" in
      advance buf 3;
      let lo_min = limit buf in
      resize buf ~len:3;
      restore lo_min buf;
      String.equal "123abc" (to_string buf)
    ;;
  end

  module Hi_bound = struct
    type t = Hi_bound.t [@@deriving compare]

    let window, restore = Hi_bound.(window, restore)

    let%test _ =
      let iobuf = create ~len:2 in
      let snapshot = window iobuf in
      assert (length iobuf = 2);
      resize iobuf ~len:1;
      assert (length iobuf = 1);
      restore snapshot iobuf;
      assert (length iobuf = 2);
      (* the same snapshot can be reused *)
      resize iobuf ~len:1;
      assert (length iobuf = 1);
      restore snapshot iobuf;
      assert (length iobuf = 2);
      (* but can fail in combination with narrow *)
      resize iobuf ~len:1;
      narrow iobuf;
      assert (capacity iobuf = 1);
      assert (length iobuf = 1);
      is_error (try_with (fun () -> restore snapshot iobuf))
    ;;

    let limit = Hi_bound.limit

    let%test _ =
      let buf = of_string "123abcDEF" in
      resize buf ~len:3;
      let hi_max = limit buf in
      advance buf 3;
      restore hi_max buf;
      String.equal "abcDEF" (to_string buf)
    ;;
  end

  let rewind = rewind
  let reset = reset
  let flip_lo = flip_lo
  let bounded_flip_lo = bounded_flip_lo

  let%test _ =
    let buf = of_string "123abcDEF" in
    Iobuf.advance buf 3;
    let lo = Lo_bound.window buf in
    Iobuf.advance buf 3;
    bounded_flip_lo buf lo;
    String.equal "abc" (Iobuf.to_string buf)
  ;;

  let%test _ =
    let buf = of_string "123abcDEF" in
    Iobuf.resize buf ~len:3;
    Iobuf.flip_hi buf;
    String.equal "abcDEF" (Iobuf.to_string buf)
  ;;

  let%test _ =
    let buf = of_string "123abcDEF" in
    Iobuf.resize buf ~len:6;
    Iobuf.narrow buf;
    Iobuf.resize buf ~len:3;
    Iobuf.flip_hi buf;
    String.equal "abc" (Iobuf.to_string buf)
  ;;

  let bounded_flip_hi = bounded_flip_hi

  let%test _ =
    let buf = of_string "123abcDEF" in
    let hi = Hi_bound.window buf in
    Iobuf.advance buf 3;
    Iobuf.resize buf ~len:3;
    bounded_flip_hi buf hi;
    String.equal "DEF" (Iobuf.to_string buf)
  ;;

  let sub_shared = sub_shared
  let set_bounds_and_buffer_sub = set_bounds_and_buffer_sub
  let set_bounds_and_buffer = set_bounds_and_buffer
  let narrow = narrow

  let%test _ =
    let buf = of_string "123" in
    assert (capacity buf = 3);
    assert (length buf = 3);
    advance buf 1;
    assert (capacity buf = 3);
    assert (length buf = 2);
    resize buf ~len:1;
    assert (capacity buf = 3);
    assert (length buf = 1);
    narrow buf;
    assert (capacity buf = 1);
    assert (length buf = 1);
    advance buf 1;
    assert (capacity buf = 1);
    assert (length buf = 0);
    reset buf;
    assert (capacity buf = 1);
    length buf = 1
  ;;

  let%test _ =
    let src = of_string "123abcDEF" in
    let dst = Iobuf.create ~len:0 in
    set_bounds_and_buffer ~src ~dst;
    src = dst
  ;;

  let%test _ =
    let src = of_string "123abcDEF" in
    let dst = Iobuf.create ~len:0 in
    set_bounds_and_buffer_sub ~src ~dst ~len:(length src) ~pos:0;
    src = dst
  ;;

  let%test _ =
    let src = of_string "123abcDEF" in
    let src = Iobuf.sub_shared ~pos:1 ~len:5 src in
    let dst = Iobuf.create ~len:0 in
    set_bounds_and_buffer ~src ~dst;
    src = dst
  ;;

  let%test _ =
    let src = of_string "123abcDEF" in
    let src = Iobuf.sub_shared ~pos:1 ~len:5 src in
    let dst = Iobuf.create ~len:0 in
    set_bounds_and_buffer_sub ~src ~dst ~len:(length src) ~pos:0;
    src = dst
  ;;

  let%test _ =
    let src = of_string "123abcDEF" in
    let src_sub = Iobuf.sub_shared ~pos:1 ~len:5 src in
    let dst = Iobuf.create ~len:0 in
    set_bounds_and_buffer_sub ~pos:2 ~len:2 ~src:src_sub ~dst;
    let src_sub' = Iobuf.sub_shared ~pos:3 ~len:2 src in
    src_sub' = dst
  ;;

  let%test _ =
    let buf1 = of_string "123abcDEF" in
    let buf2 = of_string "123abcDEF" in
    Iobuf.set_bounds_and_buffer ~src:buf1 ~dst:buf1;
    buf1 = buf2
  ;;

  let bounded_compact = bounded_compact

  let%test _ =
    let buf = of_string "123abcDEFghiJKL" in
    advance buf 3;
    let lo = Lo_bound.window buf in
    Iobuf.resize buf ~len:9;
    let hi = Hi_bound.window buf in
    advance buf 6;
    bounded_compact buf lo hi;
    assert (String.equal "DEFghi" (Iobuf.to_string buf));
    Iobuf.reset buf;
    String.equal "123ghiDEFghiJKL" (Iobuf.to_string buf)
  ;;

  let resize = resize
  let unsafe_resize = unsafe_resize

  let%test_unit _ =
    List.iter
      [ resize, true; unsafe_resize, false ]
      ~f:(fun (resize, test_invalid_access) ->
        let buf = of_string "123abcDEF" in
        if test_invalid_access
        then (
          let resize_fails amount =
            is_error (try_with (fun () -> resize buf ~len:amount))
          in
          assert (resize_fails (-1));
          assert (resize_fails (Iobuf.length buf + 1)));
        let sub = ref "" in
        let f (buf : (read, seek) t) x =
          advance buf x;
          resize buf ~len:x;
          sub := to_string buf
        in
        let check_result_and_reset () =
          assert (String.equal (to_string buf) "123abcDEF" && String.equal !sub "abc");
          sub := ""
        in
        Iobuf.protect_window_bounds_and_buffer buf ~f:(fun buf -> f buf 3);
        check_result_and_reset ();
        Iobuf.protect_window_bounds_and_buffer_1 buf 3 ~f;
        check_result_and_reset ())
  ;;

  let%test_unit _ =
    let buf = of_string "123abcDEF" in
    let sub = ref "" in
    let f (buf : (read, seek) t) x =
      advance buf x;
      resize buf ~len:x;
      sub := to_string buf;
      raise Stdlib.Not_found
    in
    let test_and_reset f =
      try Nothing.unreachable_code (f ()) with
      | Not_found_s _ | Stdlib.Not_found ->
        assert (String.equal (to_string buf) "123abcDEF" && String.equal !sub "abc");
        sub := ""
    in
    test_and_reset (fun () ->
      Iobuf.protect_window_bounds_and_buffer buf ~f:(fun buf -> f buf 3));
    test_and_reset (fun () -> Iobuf.protect_window_bounds_and_buffer_1 buf 3 ~f)
  ;;

  let%test_unit _ =
    let test t =
      let n = length t in
      rewind t;
      assert (length t = n);
      advance t 1;
      assert (length t = n - 1);
      rewind t;
      assert (length t = n);
      advance t 1;
      assert (length t = n - 1);
      rewind t;
      assert (length t = n);
      rewind t
    in
    test (create ~len:10);
    let t = create ~len:10 in
    sub_shared t ~pos:1 |> test
  ;;

  let to_string = to_string
  let of_string = of_string

  let%test_unit _ =
    List.iter strings ~f:(fun string ->
      assert (String.equal string (to_string (of_string string))))
  ;;

  let to_bytes = to_bytes
  let of_bytes = of_bytes

  let%test_unit _ =
    List.iter (List.map strings ~f:Bytes.of_string) ~f:(fun bytes ->
      assert (Bytes.equal bytes (to_bytes (of_bytes bytes))))
  ;;

  let%expect_test "to_bytes with length" =
    printf !"%{Bytes}" (to_bytes ~len:2 (of_string "abc"));
    [%expect {| ab |}]
  ;;

  let of_bigstring = of_bigstring

  let%test_unit _ =
    List.iter strings ~f:(fun string ->
      let bigstring = Bigstring.of_string string in
      iter_slices (String.length string) ~f:(fun ?pos ?len ~pos':_ ~len':_ ~is_valid () ->
        match try_with (fun () -> of_bigstring bigstring ?pos ?len) with
        | Error _ -> assert (not is_valid)
        | Ok t ->
          assert is_valid;
          assert (String.equal (to_string t) (Bigstring.to_string bigstring ?pos ?len))))
  ;;

  let advance = advance
  let unsafe_advance = unsafe_advance

  let%test_unit _ =
    List.iter
      [ advance, true; unsafe_advance, false ]
      ~f:(fun (advance, test_invalid_access) ->
        for len = 1 to 5 do
          let t = create ~len in
          let advance_fails amount = is_error (try_with (fun () -> advance t amount)) in
          if test_invalid_access
          then (
            assert (advance_fails (-1));
            assert (advance_fails (len + 1)));
          for amount = 0 to len do
            sub_shared t
            |> fun t ->
            advance t amount;
            assert (length t = len - amount)
          done
        done)
  ;;

  let consume_bin_prot = Bin_io.consume_with_header
  let fill_bin_prot = Bin_io.fill_with_header

  let%test_unit _ =
    let t = create ~len:2000 in
    List.iter strings ~f:(fun s ->
      assert (is_ok (fill_bin_prot t String.bin_writer_t s));
      flip_lo t;
      let s' = ok_exn (consume_bin_prot t String.bin_reader_t) in
      reset t;
      assert (String.equal s s'))
  ;;

  module Example_bin_prot = struct
    type t =
      | A
      | B of int
      | C of string
      | D of
          { a : int
          ; b : int
          }
    [@@deriving sexp, bin_io ~localize, quickcheck, equal]

    let quickcheck =
      Quickcheck.test
        ~shrinker:quickcheck_shrinker
        ~sexp_of:sexp_of_t
        quickcheck_generator
    ;;
  end

  let%test_unit "fill_bin_prot and fill_bin_prot_local write the same bytes" =
    Example_bin_prot.quickcheck ~f:(fun a ->
      let len = Example_bin_prot.bin_size_t a + Bin_io.header_bytes in
      let t = create ~len in
      let t' = create ~len in
      Bin_io.fill_with_header t Example_bin_prot.bin_writer_t a |> ok_exn;
      Bin_io.fill_with_header__local
        t'
        Example_bin_prot.bin_size_t__local
        Example_bin_prot.bin_write_t__local
        a
      |> ok_exn;
      flip_lo t;
      flip_lo t';
      assert (length t = len);
      assert (length t = length t');
      assert (Bytes.equal (to_bytes t) (to_bytes t')))
  ;;

  let%test_unit "fill_bin_prot roundtrips" =
    Example_bin_prot.quickcheck ~f:(fun a ->
      let len = Example_bin_prot.bin_size_t a + Bin_io.header_bytes in
      let t = create ~len in
      Bin_io.fill_with_header t Example_bin_prot.bin_writer_t a |> ok_exn;
      flip_lo t;
      let a' = consume_bin_prot t Example_bin_prot.bin_reader_t |> ok_exn in
      assert (Example_bin_prot.equal a a'))
  ;;

  let%expect_test "fill_bin_prot_local doesn't allocate" =
    let t = create ~len:2000 in
    let a : Example_bin_prot.t = D { a = 1; b = 2 } in
    require_no_allocation (fun () ->
      Bin_io.fill_with_header__local
        t
        Example_bin_prot.bin_size_t__local
        Example_bin_prot.bin_write_t__local
        a
      |> ok_exn);
    [%expect {| |}]
  ;;

  module type Accessee = sig
    type t [@@deriving sexp_of, globalize]

    include Stringable with type t := t

    val equal : t -> t -> bool
  end

  module Intf_common (Intf : sig
      include Iobuf.Accessors_common

      val accessor_pos_1
        :  without_value:(read_write, seek) Iobuf.t
        -> value_len:int
        -> ('a, read_write, seek) t
        -> value:'a
        -> with_value:string
        -> (module Accessee with type t = 'a)
        -> unit

      val bin_prot_char : (char, 'd, 'w) t
    end) :
    Iobuf.Accessors_common
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Intf.t
    with type ('a, 'b, 'c) t__local = ('a, 'b, 'c) Intf.t__local = struct
    open Intf

    type nonrec ('a, 'd, 'w) t = ('a, 'd, 'w) Intf.t
    type nonrec ('a, 'd, 'w) t__local = ('a, 'd, 'w) Intf.t__local

    let char = char

    let%test_unit _ =
      let buf = of_string "ABCDEFGHIJ" in
      let char_pos_1 a v s =
        accessor_pos_1
          ~without_value:buf
          ~value_len:1
          a
          ~value:v
          ~with_value:s
          (module struct
            include Char

            let globalize = globalize_char
          end)
      in
      char_pos_1 char 'K' "AKCDEFGHIJ";
      char_pos_1 char '\254' "A\254CDEFGHIJ";
      char_pos_1 bin_prot_char 'x' "AxCDEFGHIJ"
    ;;
  end

  module Intf_read (Intf : sig
      include Iobuf.Accessors_read

      val accessor_pos_1
        :  without_value:(read_write, seek) Iobuf.t
        -> value_len:int
        -> ('a, read_write, seek) t
        -> value:'a
        -> with_value:string
        -> (module Accessee with type t = 'a)
        -> unit

      val bin_prot_char : (char, 'd, 'w) t
    end) :
    Iobuf.Accessors_read
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Intf.t
     and type ('a, 'b, 'c) t__local = ('a, 'b, 'c) Intf.t__local = struct
    open Intf

    let int8 = int8
    let uint8 = uint8
    let int16_be = int16_be
    let int16_le = int16_le
    let uint16_be = uint16_be
    let uint16_le = uint16_le
    let int32_be = int32_be
    let int32_le = int32_le
    let uint32_be = uint32_be
    let uint32_le = uint32_le
    let int64_be_exn = int64_be_exn
    let int64_le_exn = int64_le_exn
    let int64_be_trunc = int64_be_trunc
    let int64_le_trunc = int64_le_trunc
    let uint64_be_exn = uint64_be_exn
    let uint64_le_exn = uint64_le_exn
    let int64_t_be = int64_t_be
    let int64_t_le = int64_t_le
    let tail_padded_fixed_string = tail_padded_fixed_string
    let head_padded_fixed_string = head_padded_fixed_string
    let bytes = bytes
    let string = string
    let bigstring = bigstring
    let byteso = byteso
    let stringo = stringo
    let bigstringo = bigstringo

    module Local = struct
      open Local

      let int64_t_be = int64_t_be
      let int64_t_le = int64_t_le
      let tail_padded_fixed_string = tail_padded_fixed_string
      let head_padded_fixed_string = head_padded_fixed_string
      let bytes = bytes
      let string = string
      let byteso = byteso
      let stringo = stringo
    end

    let%test_unit _ =
      let buf = of_string "ABCDEFGHIJ" in
      let int_pos_1 n a v s =
        accessor_pos_1
          ~without_value:buf
          ~value_len:n
          a
          ~value:v
          ~with_value:s
          (module struct
            include Int

            let globalize = globalize_int
          end)
      in
      int_pos_1 1 int8 127 "A\127CDEFGHIJ";
      int_pos_1 1 int8 (-1) "A\255CDEFGHIJ";
      int_pos_1 1 int8 (-128) "A\128CDEFGHIJ";
      int_pos_1 1 uint8 0 "A\000CDEFGHIJ";
      int_pos_1 1 uint8 255 "A\255CDEFGHIJ";
      int_pos_1 1 int8 (-1) "A\255CDEFGHIJ";
      int_pos_1 1 uint8 12 "A\012CDEFGHIJ";
      int_pos_1 2 int16_be 0x0102 "A\001\002DEFGHIJ";
      int_pos_1 2 int16_le 0x0304 "A\004\003DEFGHIJ";
      int_pos_1 2 int16_be (-2) "A\255\254DEFGHIJ";
      int_pos_1 2 int16_le (-3) "A\253\255DEFGHIJ";
      int_pos_1 2 uint16_be 0xFFFE "A\255\254DEFGHIJ";
      int_pos_1 2 uint16_le 0xFDFC "A\252\253DEFGHIJ";
      int_pos_1 2 int16_be 0x0506 "A\005\006DEFGHIJ";
      int_pos_1 2 int16_le 0x0708 "A\008\007DEFGHIJ";
      int_pos_1 4 int32_be 0x0A0B0C0D "A\010\011\012\013FGHIJ";
      int_pos_1 4 int32_le 0x01020304 "A\004\003\002\001FGHIJ";
      int_pos_1 4 int32_be (-0x01020305) "A\254\253\252\251FGHIJ";
      int_pos_1 4 int32_le (-0x05060709) "A\247\248\249\250FGHIJ";
      if arch_sixtyfour
      then (
        int_pos_1 4 uint32_be (large_int 0 0 0xF6F5 0xF4F3) "A\246\245\244\243FGHIJ";
        int_pos_1 4 uint32_le (large_int 0 0 0xFBFA 0xF9F8) "A\248\249\250\251FGHIJ";
        int_pos_1
          8
          int64_be_exn
          (large_int 0x0102 0x0304 0x0506 0x0708)
          "A\001\002\003\004\005\006\007\008J";
        int_pos_1
          8
          int64_le_exn
          (large_int 0x090a 0x0b0c 0x0d0e 0x0f10)
          "A\016\015\014\013\012\011\010\009J";
        int_pos_1
          8
          int64_be_exn
          (-large_int 0x0102 0x0304 0x0506 0x0709)
          "A\254\253\252\251\250\249\248\247J";
        int_pos_1
          8
          int64_le_exn
          (-large_int 0x0102 0x0304 0x0506 0x0709)
          "A\247\248\249\250\251\252\253\254J";
        int_pos_1
          8
          int64_be_trunc
          (large_int 0x0102 0x0304 0x0506 0x0708)
          "A\001\002\003\004\005\006\007\008J";
        int_pos_1
          8
          int64_le_trunc
          (large_int 0x090a 0x0b0c 0x0d0e 0x0f10)
          "A\016\015\014\013\012\011\010\009J";
        int_pos_1
          8
          int64_be_trunc
          (-large_int 0x0102 0x0304 0x0506 0x0709)
          "A\254\253\252\251\250\249\248\247J";
        int_pos_1
          8
          int64_le_trunc
          (-large_int 0x0102 0x0304 0x0506 0x0709)
          "A\247\248\249\250\251\252\253\254J")
    ;;

    module type String_accessee = sig
      include Accessee

      val subo : (t, t) Base.Blit.subo_global
    end

    module Bigstringable : String_accessee with type t = bigstring = struct
      include Bigstring

      let globalize _ = failwith "globalize is not supported on bigstrings"
      let to_string t = to_string t (* drop optional args *)
      let of_string s = of_string s (* drop optional args *)
    end

    let%test_unit _ =
      let buf = of_string "ABCDEFGHIJ" in
      let metastring_pos_1
        (type v)
        (module Value : String_accessee with type t = v)
        value_len
        accessor
        ~skip
        value
        with_value
        =
        accessor_pos_1
          ~without_value:buf
          ~value_len
          accessor
          ~value:(Value.of_string value)
          ~with_value
          (module struct
            include Value

            (* The prefix is garbage if [str_pos <> 0]. *)
            let equal t u = equal (subo ~pos:skip t) (subo ~pos:skip u)
          end)
      in
      let string_pos_1 =
        metastring_pos_1
          (module struct
            include String

            let globalize = globalize_string
          end)
      in
      let bigstring_pos_1 = metastring_pos_1 (module Bigstringable) in
      let padded_string_pos_1 ~len str ~head ~tail =
        accessor_pos_1
          ~without_value:buf
          ~value_len:len
          (tail_padded_fixed_string ~padding:'p' ~len)
          ~value:str
          ~with_value:tail
          (module struct
            include String

            let globalize = globalize_string
          end);
        accessor_pos_1
          ~without_value:buf
          ~value_len:len
          (head_padded_fixed_string ~padding:'p' ~len)
          ~value:str
          ~with_value:head
          (module struct
            include String

            let globalize = globalize_string
          end)
      in
      padded_string_pos_1 ~len:3 "x" ~head:"AppxEFGHIJ" ~tail:"AxppEFGHIJ";
      padded_string_pos_1 ~len:3 "xxx" ~head:"AxxxEFGHIJ" ~tail:"AxxxEFGHIJ";
      padded_string_pos_1 ~len:3 "xpx" ~head:"AxpxEFGHIJ" ~tail:"AxpxEFGHIJ";
      padded_string_pos_1 ~len:3 "" ~head:"ApppEFGHIJ" ~tail:"ApppEFGHIJ";
      string_pos_1 3 (string ~str_pos:0 ~len:3) ~skip:0 "123" "A123EFGHIJ";
      bigstring_pos_1 3 (bigstring ~str_pos:0 ~len:3) ~skip:0 "klm" "AklmEFGHIJ";
      string_pos_1 2 (string ~str_pos:1 ~len:2) ~skip:1 "\00023" "A23mEFGHIJ";
      bigstring_pos_1 2 (bigstring ~str_pos:1 ~len:2) ~skip:1 "\000lm" "AlmmEFGHIJ";
      string_pos_1 3 (stringo ?str_pos:None ~len:3) ~skip:0 "123" "A123EFGHIJ";
      bigstring_pos_1 3 (bigstringo ?str_pos:None ~len:3) ~skip:0 "klm" "AklmEFGHIJ";
      string_pos_1 3 (stringo ~str_pos:0 ~len:3) ~skip:0 "123" "A123EFGHIJ";
      bigstring_pos_1 3 (bigstringo ~str_pos:0 ~len:3) ~skip:0 "klm" "AklmEFGHIJ";
      string_pos_1 2 (stringo ~str_pos:1 ~len:2) ~skip:1 "\00023" "A23mEFGHIJ";
      bigstring_pos_1 2 (bigstringo ~str_pos:1 ~len:2) ~skip:1 "\000lm" "AlmmEFGHIJ"
    ;;

    include Intf_common (Intf)
    module Int_repr = Intf.Int_repr

    let%test_unit _ =
      let buf = of_string "ABCDEFGHIJ" in
      let int_pos_1 (type i) n a v (module I : Accessee with type t = i) s =
        accessor_pos_1 ~without_value:buf ~value_len:n a ~value:v ~with_value:s (module I)
      in
      int_pos_1
        1
        Int_repr.int8
        (IR_Int8.of_base_int_exn 127)
        (module IR_Int8)
        "A\127CDEFGHIJ";
      int_pos_1
        1
        Int_repr.int8
        (IR_Int8.of_base_int_exn (-1))
        (module IR_Int8)
        "A\255CDEFGHIJ";
      int_pos_1
        1
        Int_repr.int8
        (IR_Int8.of_base_int_exn (-128))
        (module IR_Int8)
        "A\128CDEFGHIJ";
      int_pos_1
        1
        Int_repr.uint8
        (IR_Uint8.of_base_int_exn 0)
        (module IR_Uint8)
        "A\000CDEFGHIJ";
      int_pos_1
        1
        Int_repr.uint8
        (IR_Uint8.of_base_int_exn 255)
        (module IR_Uint8)
        "A\255CDEFGHIJ";
      int_pos_1
        1
        Int_repr.int8
        (IR_Int8.of_base_int_exn (-1))
        (module IR_Int8)
        "A\255CDEFGHIJ";
      int_pos_1
        1
        Int_repr.uint8
        (IR_Uint8.of_base_int_exn 12)
        (module IR_Uint8)
        "A\012CDEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_be
        (IR_Int16.of_base_int_exn 0x0102)
        (module IR_Int16)
        "A\001\002DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_le
        (IR_Int16.of_base_int_exn 0x0304)
        (module IR_Int16)
        "A\004\003DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_be
        (IR_Int16.of_base_int_exn (-2))
        (module IR_Int16)
        "A\255\254DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_le
        (IR_Int16.of_base_int_exn (-3))
        (module IR_Int16)
        "A\253\255DEFGHIJ";
      int_pos_1
        2
        Int_repr.uint16_be
        (IR_Uint16.of_base_int_exn 0xFFFE)
        (module IR_Uint16)
        "A\255\254DEFGHIJ";
      int_pos_1
        2
        Int_repr.uint16_le
        (IR_Uint16.of_base_int_exn 0xFDFC)
        (module IR_Uint16)
        "A\252\253DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_be
        (IR_Int16.of_base_int_exn 0x0506)
        (module IR_Int16)
        "A\005\006DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_le
        (IR_Int16.of_base_int_exn 0x0708)
        (module IR_Int16)
        "A\008\007DEFGHIJ";
      int_pos_1
        4
        Int_repr.int32_be
        (IR_Int32.of_base_int32 0x0A0B0C0Dl)
        (module IR_Int32)
        "A\010\011\012\013FGHIJ";
      int_pos_1
        4
        Int_repr.int32_le
        (IR_Int32.of_base_int32 0x01020304l)
        (module IR_Int32)
        "A\004\003\002\001FGHIJ";
      int_pos_1
        4
        Int_repr.int32_be
        (IR_Int32.of_base_int32 (-0x01020305l))
        (module IR_Int32)
        "A\254\253\252\251FGHIJ";
      int_pos_1
        4
        Int_repr.int32_le
        (IR_Int32.of_base_int32 (-0x05060709l))
        (module IR_Int32)
        "A\247\248\249\250FGHIJ";
      if arch_sixtyfour
      then (
        int_pos_1
          4
          Int_repr.uint32_be
          (IR_Uint32.of_base_int32_trunc
             (Int32.of_int_trunc (large_int 0 0 0xF6F5 0xF4F3)))
          (module IR_Uint32)
          "A\246\245\244\243FGHIJ";
        int_pos_1
          4
          Int_repr.uint32_le
          (IR_Uint32.of_base_int32_trunc
             (Int32.of_int_trunc (large_int 0 0 0xFBFA 0xF9F8)))
          (module IR_Uint32)
          "A\248\249\250\251FGHIJ";
        int_pos_1
          8
          Int_repr.int64_be
          (Int64.of_int (large_int 0x0102 0x0304 0x0506 0x0708))
          (module IR_Int64)
          "A\001\002\003\004\005\006\007\008J";
        int_pos_1
          8
          Int_repr.int64_le
          (Int64.of_int (large_int 0x090a 0x0b0c 0x0d0e 0x0f10))
          (module IR_Int64)
          "A\016\015\014\013\012\011\010\009J";
        int_pos_1
          8
          Int_repr.int64_be
          (Int64.of_int (-large_int 0x0102 0x0304 0x0506 0x0709))
          (module IR_Int64)
          "A\254\253\252\251\250\249\248\247J";
        int_pos_1
          8
          Int_repr.int64_le
          (Int64.of_int (-large_int 0x0102 0x0304 0x0506 0x0709))
          (module IR_Int64)
          "A\247\248\249\250\251\252\253\254J")
    ;;
  end

  module Intf_write (Intf : sig
      include Iobuf.Accessors_write

      val accessor_pos_1
        :  without_value:(read_write, seek) Iobuf.t
        -> value_len:int
        -> ('a, read_write, seek) t
        -> value:'a
        -> with_value:string
        -> (module Accessee with type t = 'a)
        -> unit

      val bin_prot_char : (char, 'd, 'w) t
      val ignore_locality : ('a, 'd, 'w) t__local -> ('a, 'd, 'w) t
    end) :
    Iobuf.Accessors_write
    with type ('a, 'b, 'c) t = ('a, 'b, 'c) Intf.t
     and type ('a, 'b, 'c) t__local = ('a, 'b, 'c) Intf.t__local = struct
    open Intf

    let int8_trunc = int8_trunc
    let uint8_trunc = uint8_trunc
    let int16_be_trunc = int16_be_trunc
    let int16_le_trunc = int16_le_trunc
    let uint16_be_trunc = uint16_be_trunc
    let uint16_le_trunc = uint16_le_trunc
    let int32_be_trunc = int32_be_trunc
    let int32_le_trunc = int32_le_trunc
    let uint32_be_trunc = uint32_be_trunc
    let uint32_le_trunc = uint32_le_trunc
    let int64_be = int64_be
    let int64_le = int64_le
    let uint64_be_trunc = uint64_be_trunc
    let uint64_le_trunc = uint64_le_trunc
    let int64_t_be = int64_t_be
    let int64_t_le = int64_t_le
    let tail_padded_fixed_string = tail_padded_fixed_string
    let head_padded_fixed_string = head_padded_fixed_string
    let bytes = bytes
    let string = string
    let bigstring = bigstring
    let byteso = byteso
    let stringo = stringo
    let bigstringo = bigstringo

    let%test_unit _ =
      let buf = of_string "ABCDEFGHIJ" in
      let int_pos_1 n a v s =
        accessor_pos_1
          ~without_value:buf
          ~value_len:n
          a
          ~value:v
          ~with_value:s
          (module struct
            include Int

            let globalize = globalize_int
          end)
      in
      int_pos_1 1 int8_trunc 127 "A\127CDEFGHIJ";
      int_pos_1 1 int8_trunc (-1) "A\255CDEFGHIJ";
      int_pos_1 1 int8_trunc (-128) "A\128CDEFGHIJ";
      int_pos_1 1 uint8_trunc 0 "A\000CDEFGHIJ";
      int_pos_1 1 uint8_trunc 255 "A\255CDEFGHIJ";
      int_pos_1 1 int8_trunc (-1) "A\255CDEFGHIJ";
      int_pos_1 1 uint8_trunc 12 "A\012CDEFGHIJ";
      int_pos_1 2 int16_be_trunc 0x0102 "A\001\002DEFGHIJ";
      int_pos_1 2 int16_le_trunc 0x0304 "A\004\003DEFGHIJ";
      int_pos_1 2 int16_be_trunc (-2) "A\255\254DEFGHIJ";
      int_pos_1 2 int16_le_trunc (-3) "A\253\255DEFGHIJ";
      int_pos_1 2 uint16_be_trunc 0xFFFE "A\255\254DEFGHIJ";
      int_pos_1 2 uint16_le_trunc 0xFDFC "A\252\253DEFGHIJ";
      int_pos_1 2 int16_be_trunc 0x0506 "A\005\006DEFGHIJ";
      int_pos_1 2 int16_le_trunc 0x0708 "A\008\007DEFGHIJ";
      int_pos_1 4 int32_be_trunc 0x0A0B0C0D "A\010\011\012\013FGHIJ";
      int_pos_1 4 int32_le_trunc 0x01020304 "A\004\003\002\001FGHIJ";
      int_pos_1 4 int32_be_trunc (-0x01020305) "A\254\253\252\251FGHIJ";
      int_pos_1 4 int32_le_trunc (-0x05060709) "A\247\248\249\250FGHIJ";
      if arch_sixtyfour
      then (
        int_pos_1 4 uint32_be_trunc (large_int 0 0 0xF6F5 0xF4F3) "A\246\245\244\243FGHIJ";
        int_pos_1 4 uint32_le_trunc (large_int 0 0 0xFBFA 0xF9F8) "A\248\249\250\251FGHIJ";
        int_pos_1
          8
          int64_be
          (large_int 0x0102 0x0304 0x0506 0x0708)
          "A\001\002\003\004\005\006\007\008J";
        int_pos_1
          8
          int64_le
          (large_int 0x090a 0x0b0c 0x0d0e 0x0f10)
          "A\016\015\014\013\012\011\010\009J";
        int_pos_1
          8
          int64_be
          (-large_int 0x0102 0x0304 0x0506 0x0709)
          "A\254\253\252\251\250\249\248\247J";
        int_pos_1
          8
          int64_le
          (-large_int 0x0102 0x0304 0x0506 0x0709)
          "A\247\248\249\250\251\252\253\254J")
    ;;

    module type String_accessee = sig
      include Accessee

      val subo : (t, t) Base.Blit.subo_global
    end

    module Bigstringable : String_accessee with type t = bigstring = struct
      include Bigstring

      let globalize _ = failwith "globalize is not supported on bigstrings"
      let to_string t = to_string t (* drop optional args *)
      let of_string s = of_string s (* drop optional args *)
    end

    let%test_unit _ =
      let buf = of_string "ABCDEFGHIJ" in
      let metastring_pos_1
        (type v)
        (module Value : String_accessee with type t = v)
        value_len
        accessor
        ~skip
        value
        with_value
        =
        accessor_pos_1
          ~without_value:buf
          ~value_len
          (ignore_locality accessor)
          ~value:(Value.of_string value)
          ~with_value
          (module struct
            include Value

            (* The prefix is garbage if [str_pos <> 0]. *)
            let equal t u = equal (subo ~pos:skip t) (subo ~pos:skip u)
          end) [@nontail]
      in
      let string_pos_1 =
        metastring_pos_1
          (module struct
            include String

            let globalize = globalize_string
          end)
      in
      let bigstring_pos_1 = metastring_pos_1 (module Bigstringable) in
      let padded_string_pos_1 ~len str ~head ~tail =
        accessor_pos_1
          ~without_value:buf
          ~value_len:len
          (ignore_locality (tail_padded_fixed_string ~padding:'p' ~len))
          ~value:str
          ~with_value:tail
          (module struct
            include String

            let globalize = globalize_string
          end);
        accessor_pos_1
          ~without_value:buf
          ~value_len:len
          (ignore_locality (head_padded_fixed_string ~padding:'p' ~len))
          ~value:str
          ~with_value:head
          (module struct
            include String

            let globalize = globalize_string
          end) [@nontail]
      in
      padded_string_pos_1 ~len:3 "x" ~head:"AppxEFGHIJ" ~tail:"AxppEFGHIJ";
      padded_string_pos_1 ~len:3 "xxx" ~head:"AxxxEFGHIJ" ~tail:"AxxxEFGHIJ";
      padded_string_pos_1 ~len:3 "xpx" ~head:"AxpxEFGHIJ" ~tail:"AxpxEFGHIJ";
      padded_string_pos_1 ~len:3 "" ~head:"ApppEFGHIJ" ~tail:"ApppEFGHIJ";
      string_pos_1 3 (string ~str_pos:0 ~len:3) ~skip:0 "123" "A123EFGHIJ";
      bigstring_pos_1 3 (bigstring ~str_pos:0 ~len:3) ~skip:0 "klm" "AklmEFGHIJ";
      string_pos_1 2 (string ~str_pos:1 ~len:2) ~skip:1 "\00023" "A23mEFGHIJ";
      bigstring_pos_1 2 (bigstring ~str_pos:1 ~len:2) ~skip:1 "\000lm" "AlmmEFGHIJ";
      string_pos_1 3 (stringo ?str_pos:None ~len:3) ~skip:0 "123" "A123EFGHIJ";
      bigstring_pos_1 3 (bigstringo ?str_pos:None ~len:3) ~skip:0 "klm" "AklmEFGHIJ";
      string_pos_1 3 (stringo ~str_pos:0 ~len:3) ~skip:0 "123" "A123EFGHIJ";
      bigstring_pos_1 3 (bigstringo ~str_pos:0 ~len:3) ~skip:0 "klm" "AklmEFGHIJ";
      string_pos_1 2 (stringo ~str_pos:1 ~len:2) ~skip:1 "\00023" "A23mEFGHIJ";
      bigstring_pos_1 2 (bigstringo ~str_pos:1 ~len:2) ~skip:1 "\000lm" "AlmmEFGHIJ"
    ;;

    include Intf_common (Intf)
    module Int_repr = Intf.Int_repr

    let%test_unit _ =
      let buf = of_string "ABCDEFGHIJ" in
      let int_pos_1 (type i) n a v (module I : Accessee with type t = i) s =
        accessor_pos_1 ~without_value:buf ~value_len:n a ~value:v ~with_value:s (module I)
      in
      int_pos_1
        1
        Int_repr.int8
        (IR_Int8.of_base_int_exn 127)
        (module IR_Int8)
        "A\127CDEFGHIJ";
      int_pos_1
        1
        Int_repr.int8
        (IR_Int8.of_base_int_exn (-1))
        (module IR_Int8)
        "A\255CDEFGHIJ";
      int_pos_1
        1
        Int_repr.int8
        (IR_Int8.of_base_int_exn (-128))
        (module IR_Int8)
        "A\128CDEFGHIJ";
      int_pos_1
        1
        Int_repr.uint8
        (IR_Uint8.of_base_int_exn 0)
        (module IR_Uint8)
        "A\000CDEFGHIJ";
      int_pos_1
        1
        Int_repr.uint8
        (IR_Uint8.of_base_int_exn 255)
        (module IR_Uint8)
        "A\255CDEFGHIJ";
      int_pos_1
        1
        Int_repr.int8
        (IR_Int8.of_base_int_exn (-1))
        (module IR_Int8)
        "A\255CDEFGHIJ";
      int_pos_1
        1
        Int_repr.uint8
        (IR_Uint8.of_base_int_exn 12)
        (module IR_Uint8)
        "A\012CDEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_be
        (IR_Int16.of_base_int_exn 0x0102)
        (module IR_Int16)
        "A\001\002DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_le
        (IR_Int16.of_base_int_exn 0x0304)
        (module IR_Int16)
        "A\004\003DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_be
        (IR_Int16.of_base_int_exn (-2))
        (module IR_Int16)
        "A\255\254DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_le
        (IR_Int16.of_base_int_exn (-3))
        (module IR_Int16)
        "A\253\255DEFGHIJ";
      int_pos_1
        2
        Int_repr.uint16_be
        (IR_Uint16.of_base_int_exn 0xFFFE)
        (module IR_Uint16)
        "A\255\254DEFGHIJ";
      int_pos_1
        2
        Int_repr.uint16_le
        (IR_Uint16.of_base_int_exn 0xFDFC)
        (module IR_Uint16)
        "A\252\253DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_be
        (IR_Int16.of_base_int_exn 0x0506)
        (module IR_Int16)
        "A\005\006DEFGHIJ";
      int_pos_1
        2
        Int_repr.int16_le
        (IR_Int16.of_base_int_exn 0x0708)
        (module IR_Int16)
        "A\008\007DEFGHIJ";
      int_pos_1
        4
        Int_repr.int32_be
        (IR_Int32.of_base_int32 0x0A0B0C0Dl)
        (module IR_Int32)
        "A\010\011\012\013FGHIJ";
      int_pos_1
        4
        Int_repr.int32_le
        (IR_Int32.of_base_int32 0x01020304l)
        (module IR_Int32)
        "A\004\003\002\001FGHIJ";
      int_pos_1
        4
        Int_repr.int32_be
        (IR_Int32.of_base_int32 (-0x01020305l))
        (module IR_Int32)
        "A\254\253\252\251FGHIJ";
      int_pos_1
        4
        Int_repr.int32_le
        (IR_Int32.of_base_int32 (-0x05060709l))
        (module IR_Int32)
        "A\247\248\249\250FGHIJ";
      if arch_sixtyfour
      then (
        int_pos_1
          4
          Int_repr.uint32_be
          (IR_Uint32.of_base_int32_trunc 0xF6F5F4F3l)
          (module IR_Uint32)
          "A\246\245\244\243FGHIJ";
        int_pos_1
          4
          Int_repr.uint32_le
          (IR_Uint32.of_base_int32_trunc 0xFBFAF9F8l)
          (module IR_Uint32)
          "A\248\249\250\251FGHIJ";
        int_pos_1
          8
          Int_repr.int64_be
          (Int64.of_int (large_int 0x0102 0x0304 0x0506 0x0708))
          (module IR_Int64)
          "A\001\002\003\004\005\006\007\008J";
        int_pos_1
          8
          Int_repr.int64_le
          (Int64.of_int (large_int 0x090a 0x0b0c 0x0d0e 0x0f10))
          (module IR_Int64)
          "A\016\015\014\013\012\011\010\009J";
        int_pos_1
          8
          Int_repr.int64_be
          (Int64.of_int (-large_int 0x0102 0x0304 0x0506 0x0709))
          (module IR_Int64)
          "A\254\253\252\251\250\249\248\247J";
        int_pos_1
          8
          Int_repr.int64_le
          (Int64.of_int (-large_int 0x0102 0x0304 0x0506 0x0709))
          (module IR_Int64)
          "A\247\248\249\250\251\252\253\254J")
    ;;
  end

  let cases_for_testing_decimal =
    [ Int.min_value
    ; Int.min_value + 1
    ; -100
    ; -9
    ; 0
    ; 10
    ; 15
    ; 45
    ; 120
    ; 987
    ; 2814
    ; 10000
    ; 100_000
    ; 1_000_000
    ; Int.max_value
    ]
    @ List.init 1_000 ~f:(fun _ -> Random.int (1 lsl 29) lsl Random.int 35)
  ;;

  let lengths_for_testing_padded_decimal = [ 0; 1; 5; 30 ]

  module Poke = struct
    include Intf_write (struct
        include Poke

        let accessor_pos_1
          (type a)
          ~without_value:buf
          ~value_len:_
          f
          ~value:arg
          ~with_value:str
          (module Arg : Accessee with type t = a)
          =
          f buf ~pos:1 arg;
          let buf_str = to_string buf in
          if String.( <> ) str buf_str
          then
            raise_s
              [%message
                "Poke accessor_pos_1 failed"
                  (str : string)
                  (buf_str : string)
                  (arg : Arg.t)]
        ;;

        let bin_prot_char t ~pos a = Bin_io.poke Char.bin_writer_t t ~pos a
        let ignore_locality (f : ('a, 'd, 'w) t__local) = (f :> ('a, 'd, 'w) t)

        (* Static permission tests for the cases that do compile.  Since the functions
           all use essentially the same type definitions, we don't need to test all of
           them.  We've already tested them on a (read_write, seek) Iobuf.t above. *)
        let%test_unit _ =
          char (of_string "a" : (_, no_seek) Iobuf.t) ~pos:0 'b';
          char (of_string "a" : (_, seek) Iobuf.t) ~pos:0 'b';
          char (of_string "a" : (read_write, _) Iobuf.t) ~pos:0 'b'
        ;;
      end)

    let decimal = Poke.decimal

    let%test_unit "Poke.decimal" =
      let pos = 1 in
      let t = create ~len:(20 + pos) in
      List.iter cases_for_testing_decimal ~f:(fun x ->
        Iobuf.reset t;
        let len = decimal t ~pos x in
        [%test_eq: string]
          (Int.to_string x)
          (Iobuf.to_string (Iobuf.sub_shared ~pos t) ~len))
    ;;

    let padded_decimal = Poke.padded_decimal

    let%test_unit "Poke.padded_decimal" =
      let pos = 1 in
      let t = create ~len:(30 + pos) in
      List.iter cases_for_testing_decimal ~f:(fun x ->
        List.iter lengths_for_testing_padded_decimal ~f:(fun len ->
          Iobuf.reset t;
          let len = padded_decimal t ~pos ~len x in
          [%test_eq: string]
            (sprintf "%0*d" len x)
            (Iobuf.to_string (Iobuf.sub_shared ~pos t) ~len)))
    ;;

    let date_string_iso8601_extended = Poke.date_string_iso8601_extended

    let%test_unit "Poke.date_string_iso8601_extended" =
      let pos = 1 in
      let t = create ~len:(Date_string.len_iso8601_extended + pos) in
      Quickcheck.iter [%quickcheck.generator: Date.t] ~f:(fun date ->
        Iobuf.reset t;
        date_string_iso8601_extended t ~pos date;
        [%test_eq: string]
          (Date.to_string date)
          (Iobuf.to_string
             (Iobuf.sub_shared ~pos t)
             ~len:Date_string.len_iso8601_extended))
    ;;

    let bin_prot_size = Bin_io.poke_size

    let%expect_test "bin prot size" =
      let buf = of_string (String.make 32 'Z') in
      let string = "a string here" in
      let len = bin_prot_size String.bin_writer_t buf ~pos:3 string in
      let expected = String.bin_size_t string in
      assert (len = expected);
      print_s [%sexp (buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
      [%expect
        {|
        ("00000000  5a 5a 5a 0d 61 20 73 74  72 69 6e 67 20 68 65 72  |ZZZ.a string her|"
         "00000010  65 5a 5a 5a 5a 5a 5a 5a  5a 5a 5a 5a 5a 5a 5a 5a  |eZZZZZZZZZZZZZZZ|")
        |}]
    ;;
  end

  let%test_unit _ =
    let t = create ~len:10 in
    let n = length t in
    assert (n = 10);
    Poke.char t ~pos:0 'a';
    Poke.char t ~pos:1 'b';
    (sub_shared t
     |> fun t ->
     assert (length t = n);
     assert (Consume.char t = 'a'));
    assert (Consume.char t = 'a');
    assert (Consume.char t = 'b');
    let ws = (t :> (read_write, seek) t) in
    let rs = (ws :> (read, seek) t) in
    ignore (rs :> (read, no_seek) t);
    ignore (ws :> (read_write, no_seek) t)
  ;;

  let test_peek_to
    (blito : (_ Peek.src, _) Base.Blit.blito)
    create
    sub_string
    of_string
    to_string
    =
    iter_examples ~f:(fun t string ~pos ->
      let n = String.length string in
      iter_slices n ~f:(fun ?pos:str_pos ?len ~pos' ~len' ~is_valid () ->
        let fill_result =
          Or_error.try_with (fun () ->
            sub_shared t ~pos |> fun t -> Fill.stringo t string ?str_pos ?len)
        in
        [%test_eq: bool] (is_ok fill_result) is_valid;
        let str = create n in
        let consume_result =
          Or_error.try_with (fun () ->
            sub_shared (read_only (no_seek t)) ~pos
            |> fun t -> blito ~src:t ~src_pos:0 ~src_len:len' ~dst:str ?dst_pos:str_pos ())
        in
        match consume_result, is_valid with
        | Error _, false -> ()
        | Ok (), true ->
          [%test_eq: string]
            (String.sub string ~pos:pos' ~len:len')
            (sub_string str ~pos:pos' ~len:len')
        | _, _ ->
          raise_s
            [%sexp
              "test_peek_to"
              , (consume_result : unit Or_error.t)
              , ~~(is_valid : bool)
              , (Backtrace.Exn.most_recent () : Backtrace.t)]));
    let t = Iobuf.of_string "012345678" in
    let dst = of_string "abcdefhij" in
    blito ~src:t ~src_len:3 ~dst ~dst_pos:3 ();
    [%test_eq: string] (Iobuf.to_string t) "012345678";
    [%test_eq: string] (to_string dst) "abc012hij"
  ;;

  let test_peek_to_string blito =
    test_peek_to blito Bytes.create Bytes.To_string.sub Bytes.of_string Bytes.to_string
  ;;

  let test_peek_to_bigstring blito =
    test_peek_to
      blito
      Bigstring.create
      (fun s ~pos ~len -> Bigstring.to_string s ~pos ~len)
      Bigstring.of_string
      Bigstring.to_string
  ;;

  module Peek = struct
    include Intf_read (struct
        include Peek

        let accessor_pos_1
          (type a)
          ~without_value:_
          ~value_len:_
          f
          ~value:expected
          ~with_value:str
          (module Res : Accessee with type t = a)
          =
          let res = f (of_string str) ~pos:1 in
          if not (Res.equal res expected)
          then
            raise_s
              [%message
                "Peek accessor_pos_1 failed"
                  (res : Res.t)
                  (expected : Res.t)
                  (str : string)]
        ;;

        let bin_prot_char t ~pos = Bin_io.peek Char.bin_read_t t ~pos

        (* static permission tests; see above *)
        let char_a_pos_0 iobuf = Char.( = ) 'a' (char iobuf ~pos:0)
        let%test _ = char_a_pos_0 (of_string "a" : (_, no_seek) Iobuf.t)
        let%test _ = char_a_pos_0 (of_string "a" : (_, seek) Iobuf.t)
        let%test _ = char_a_pos_0 (of_string "a" : (read, _) Iobuf.t)
        let%test _ = char_a_pos_0 (of_string "a" : (read_write, _) Iobuf.t)
      end)

    let index = Peek.index
    let rindex = Peek.rindex

    module _ = struct
      open Peek.To_bytes

      let blito = blito
      let%test_unit _ = test_peek_to_string blito
    end

    module _ = struct
      open Peek.To_bigstring

      let blito = blito
      let%test_unit _ = test_peek_to_bigstring blito
    end
  end

  let%test_unit _ =
    let s = "hello" in
    let t = of_string s in
    for i = 0 to String.length s - 1 do
      assert (Char.equal (Peek.char t ~pos:i) s.[i]);
      Poke.char t ~pos:i 'z';
      assert (Char.equal (Peek.char t ~pos:i) 'z')
    done;
    if is_safe
    then (
      assert (is_error (try_with (fun () -> Peek.char t ~pos:(-1))));
      assert (is_error (try_with (fun () -> Poke.char t ~pos:(-1) 'z')));
      assert (is_error (try_with (fun () -> Peek.char t ~pos:(String.length s))));
      assert (is_error (try_with (fun () -> Poke.char t ~pos:(String.length s) 'z'))))
  ;;

  let%expect_test "index and rindex" =
    let s = "hello" in
    let t = of_string s in
    let test ?pos ?len char =
      let index = Peek.index t ?pos ?len char in
      let rindex = Peek.rindex t ?pos ?len char in
      print_s
        [%message
          (pos : (int option[@sexp.option]))
            (len : (int option[@sexp.option]))
            (char : char)
            (index : int option)
            (rindex : int option)];
      let check i =
        match i with
        | None -> require (not (String.mem (String.subo ?pos ?len s) char))
        | Some pos -> require_equal (module Char) (Peek.char t ~pos) char
      in
      check index;
      check rindex
    in
    let require_error here f =
      (* Do this instead of [require_does_raise] because the latter prints out the exn.
         We need to have the same output in the safe and unsafe variants. *)
      require_error ~here [%sexp_of: unit] (try_with f)
    in
    test 'h';
    test 'l';
    test 'l' ~pos:2;
    test 'l' ~pos:3;
    test 'l' ~pos:4;
    test 'l' ~len:3;
    test 'l' ~len:4;
    test 'o';
    test 'o' ~len:3;
    test 'z';
    [%expect
      {|
      ((char h)
       (index  (0))
       (rindex (0)))
      ((char l)
       (index  (2))
       (rindex (3)))
      ((pos  2)
       (char l)
       (index  (2))
       (rindex (3)))
      ((pos  3)
       (char l)
       (index  (3))
       (rindex (3)))
      ((pos  4)
       (char l)
       (index  ())
       (rindex ()))
      ((len  3)
       (char l)
       (index  (2))
       (rindex (2)))
      ((len  4)
       (char l)
       (index  (2))
       (rindex (3)))
      ((char o)
       (index  (4))
       (rindex (4)))
      ((len  3)
       (char o)
       (index  ())
       (rindex ()))
      ((char z)
       (index  ())
       (rindex ()))
      |}];
    if is_safe
    then (
      require_error [%here] (fun () -> test 'z' ~pos:(-1));
      require_error [%here] (fun () -> test 'z' ~pos:0 ~len:10);
      require_error [%here] (fun () -> test 'z' ~pos:4 ~len:(-1)))
  ;;

  let%test_unit _ =
    List.iter [ 0; 1 ] ~f:(fun pos ->
      let t = create ~len:10 in
      if arch_sixtyfour
      then (
        let i = large_int 0x1234 0x5678 0x90AB 0xCDEF in
        Poke.int64_le t ~pos i;
        assert (Peek.int64_le_exn t ~pos = i);
        assert (Peek.int64_le_trunc t ~pos = i);
        Poke.int64_be t ~pos i;
        assert (Peek.int64_be_exn t ~pos = i);
        assert (Peek.int64_be_trunc t ~pos = i));
      let i = 0x1234_5678 in
      Poke.int32_le_trunc t ~pos i;
      assert (Peek.int32_le t ~pos = i);
      Poke.int32_be_trunc t ~pos i;
      assert (Peek.int32_be t ~pos = i);
      let i = 0x1234 in
      Poke.int32_le_trunc t ~pos i;
      assert (Peek.int32_le t ~pos = i);
      Poke.int32_be_trunc t ~pos i;
      assert (Peek.int32_be t ~pos = i))
  ;;

  module Fill = struct
    include Intf_write (struct
        include Fill

        let accessor_pos_1
          (type a)
          ~without_value:buf
          ~value_len:n
          f
          ~value:arg
          ~with_value:str
          (module Arg : Accessee with type t = a)
          =
          rewind buf;
          advance buf 1;
          f buf arg;
          if Iobuf.length buf <> String.length str - 1 - n
          then
            raise_s
              [%message
                "Fill accessor_pos_1 post length check failed"
                  (buf : (_, _) Iobuf.Hexdump.t)
                  (str : string)
                  (n : int)
                  (arg : Arg.t)];
          rewind buf;
          let buf_str = to_string buf in
          if String.( <> ) buf_str str
          then
            raise_s
              [%message
                "Fill accessor_pos_1 post rewind check failed"
                  (buf_str : string)
                  (str : string)
                  (n : int)
                  (arg : Arg.t)]
        ;;

        let bin_prot_char t a = Bin_io.fill Char.bin_writer_t t a
        let ignore_locality (f : ('a, 'd, 'w) t__local) = (f :> ('a, 'd, 'w) t)

        let%test_unit _ =
          let t = of_string "abc" in
          Bin_io.fill Char.bin_writer_t t 'd';
          Bin_io.fill Char.bin_writer_t t 'e';
          [%test_eq: string] "c" (to_string t);
          flip_lo t;
          assert (
            try
              Bin_io.fill String.bin_writer_t t "fgh";
              false
            with
            | _ -> true);
          [%test_eq: string] "de" (to_string t);
          reset t;
          [%test_eq: string] "dec" (to_string t);
          Bin_io.fill Char.bin_writer_t t 'i';
          Bin_io.fill Char.bin_writer_t t 'j';
          Bin_io.fill Char.bin_writer_t t 'k';
          assert (is_empty t);
          flip_lo t;
          [%test_eq: string] "ijk" (to_string t)
        ;;

        (* static permission tests; see above *)
        let%test_unit _ = char (of_string "a" : (_, seek) Iobuf.t) 'b'
        let%test_unit _ = char (of_string "a" : (read_write, _) Iobuf.t) 'b'
      end)

    let decimal = Fill.decimal

    let%test_unit "Fill.decimal" =
      let t = create ~len:20 in
      List.iter cases_for_testing_decimal ~f:(fun x ->
        Iobuf.reset t;
        decimal t x;
        Iobuf.flip_lo t;
        [%test_eq: string] (Int.to_string x) (Iobuf.to_string t))
    ;;

    let padded_decimal = Fill.padded_decimal

    let%test_unit "Fill.padded_decimal" =
      let t = create ~len:30 in
      List.iter cases_for_testing_decimal ~f:(fun x ->
        List.iter lengths_for_testing_padded_decimal ~f:(fun len ->
          Iobuf.reset t;
          padded_decimal ~len t x;
          Iobuf.flip_lo t;
          [%test_eq: string] (sprintf "%0*d" len x) (Iobuf.to_string t)))
    ;;

    let date_string_iso8601_extended = Fill.date_string_iso8601_extended

    let%test_unit "Fill.date_string_iso8601_extended" =
      let t = create ~len:Date_string.len_iso8601_extended in
      Quickcheck.iter [%quickcheck.generator: Date.t] ~f:(fun date ->
        Iobuf.reset t;
        date_string_iso8601_extended t date;
        Iobuf.flip_lo t;
        [%test_eq: string] (Date.to_string date) (Iobuf.to_string t))
    ;;
  end

  let%test_unit _ =
    List.iter [ ""; "a"; "ab" ] ~f:(fun s ->
      let len = String.length s in
      for capacity = max 1 len to len + 2 do
        let t = create ~len:capacity in
        for pos = 0 to capacity - len do
          (sub_shared t ~pos |> fun t -> Fill.stringo t s);
          sub_shared t ~pos ~len
          |> fun t ->
          let s' = to_string t in
          assert (String.equal s s');
          assert (length t = len)
          (* [to_string] didn't change the length *)
        done
      done)
  ;;

  let%test_unit _ =
    let src = create ~len:5 in
    let len = 3 in
    let str = "Hi." in
    assert (len >= String.length str);
    assert (capacity src >= len);
    Poke.stringo src str ~pos:0;
    Blit.blit ~src ~src_pos:0 ~dst:src ~dst_pos:2 ~len;
    [%test_result: string] (to_string src) ~expect:"HiHi."
  ;;

  let%test_unit _ =
    let src = create ~len:5 in
    let len = 3 in
    let str = "Hi." in
    assert (len >= String.length str);
    assert (capacity src >= len);
    let dst = create ~len:5 in
    Fill.stringo src str;
    flip_lo src;
    Blit_consume_and_fill.blit ~src ~dst ~len;
    flip_lo dst;
    [%test_result: string] (Consume.stringo dst) ~expect:str
  ;;

  let test_consume_to
    (blito : (Consume.src, _) Iobuf.consuming_blito)
    create
    sub_string
    of_string
    to_string
    =
    iter_examples ~f:(fun t string ~pos ->
      let n = String.length string in
      iter_slices n ~f:(fun ?pos:str_pos ?len ~pos' ~len' ~is_valid () ->
        let fill_result =
          Or_error.try_with (fun () ->
            sub_shared t ~pos |> fun t -> Fill.stringo t string ?str_pos ?len)
        in
        [%test_eq: bool] (is_ok fill_result) is_valid;
        let str = create n in
        let consume_result =
          Or_error.try_with (fun () ->
            sub_shared (read_only t) ~pos
            |> fun t -> blito ~src:t ~src_len:len' ~dst:str ?dst_pos:str_pos ())
        in
        match consume_result, is_valid with
        | Error _, false -> ()
        | Ok (), true ->
          [%test_eq: string]
            (String.sub string ~pos:pos' ~len:len')
            (sub_string str ~pos:pos' ~len:len')
        | _, _ ->
          raise_s
            [%sexp
              "test_consume_to"
              , (consume_result : unit Or_error.t)
              , ~~(is_valid : bool)
              , (Backtrace.Exn.most_recent () : Backtrace.t)]));
    let t = Iobuf.of_string "012345678" in
    let dst = of_string "abcdefhij" in
    blito ~src:t ~src_len:3 ~dst ~dst_pos:3 ();
    [%test_eq: string] (Iobuf.to_string t) "345678";
    [%test_eq: string] (to_string dst) "abc012hij"
  ;;

  let test_consume_to_string blito =
    test_consume_to blito Bytes.create Bytes.To_string.sub Bytes.of_string Bytes.to_string
  ;;

  let test_consume_to_bigstring blito =
    test_consume_to
      blito
      Bigstring.create
      (fun s ~pos ~len -> Bigstring.to_string s ~pos ~len)
      Bigstring.of_string
      Bigstring.to_string
  ;;

  module Consume = struct
    include Intf_read (struct
        include Consume

        let accessor_pos_1
          (type r)
          ~without_value:_
          ~value_len:n
          f
          ~value:expected
          ~with_value:str
          (module Res : Accessee with type t = r)
          =
          let buf = of_string str in
          advance buf 1;
          let res = f buf in
          if Iobuf.length buf <> String.length str - 1 - n
          then
            raise_s
              [%message
                "Consume accessor_pos_1 post length check failed"
                  (buf : (_, _) Iobuf.Hexdump.t)
                  (str : string)
                  (n : int)
                  (res : Res.t)];
          rewind buf;
          if not (Res.equal res expected)
          then
            raise_s
              [%message
                "Consume accessor_pos_1 post rewind check failed"
                  (res : Res.t)
                  (expected : Res.t)
                  (str : string)
                  (n : int)]
        ;;

        let bin_prot_char t = Bin_io.consume Char.bin_reader_t t

        (* static permission tests; see above *)
        let%test _ = Char.( = ) 'a' (char (of_string "a" : (_, seek) Iobuf.t))
        let%test _ = Char.( = ) 'a' (char (of_string "a" : (read, _) Iobuf.t))
        let%test _ = Char.( = ) 'a' (char (of_string "a" : (read_write, _) Iobuf.t))
      end)

    open Consume

    module _ = struct
      open To_bytes

      let blito = blito
      let%test_unit _ = test_consume_to_string blito
    end

    module _ = struct
      open To_bigstring

      let blito = blito
      let%test_unit _ = test_consume_to_bigstring blito
    end
  end

  let%test_unit _ =
    let t = create ~len:1 in
    let c = 'a' in
    (sub_shared t
     |> fun t ->
     Fill.char t c;
     assert (is_empty t));
    assert (Char.equal (Consume.char t) c);
    assert (is_empty t)
  ;;

  module%test [@name "truncating pokers and fillers don't check values"] _ = struct
    let t = Iobuf.create ~len:8

    let tests tests =
      List.iter tests ~f:(fun test ->
        reset t;
        Poke.stringo t "01234567" ~pos:0;
        try
          test ();
          printf "%S\n" (Iobuf.to_string t)
        with
        | e -> print_s [%sexp (e : exn)])
    ;;

    let%expect_test "unsigned Pokes of negative ints" =
      tests
        (List.map
           Poke.
             [ uint8_trunc
             ; uint16_be_trunc
             ; uint16_le_trunc
             ; uint32_be_trunc
             ; uint32_le_trunc
             ; uint64_be_trunc
             ; uint64_le_trunc
             ]
           ~f:(fun upoke () -> upoke t (-1 lxor 0xFF lor Char.to_int '!') ~pos:0));
      [%expect
        {|
        "!1234567"
        "\255!234567"
        "!\255234567"
        "\255\255\255!4567"
        "!\255\255\2554567"
        "\255\255\255\255\255\255\255!"
        "!\255\255\255\255\255\255\255"
        |}]
    ;;

    let%expect_test "signed Pokes of out-of-range ints" =
      tests
        (List.map
           Poke.
             [ int8_trunc
             ; int16_be_trunc
             ; int16_le_trunc
             ; int32_be_trunc
             ; int32_le_trunc
             ]
           ~f:(fun poke () -> poke t ((1 lsl 32) lor Char.to_int '!') ~pos:0));
      [%expect
        {|
        "!1234567"
        "\000!234567"
        "!\000234567"
        "\000\000\000!4567"
        "!\000\000\0004567"
        |}]
    ;;

    let%expect_test "unsigned Fills don't check sign" =
      tests
        (List.map
           Fill.
             [ uint8_trunc
             ; uint16_be_trunc
             ; uint16_le_trunc
             ; uint32_be_trunc
             ; uint32_le_trunc
             ; uint64_be_trunc
             ; uint64_le_trunc
             ]
           ~f:(fun ufill () ->
             ufill t (-1 lxor 0xFF lor Char.to_int '!');
             Iobuf.flip_lo t));
      [%expect
        {|
        "!"
        "\255!"
        "!\255"
        "\255\255\255!"
        "!\255\255\255"
        "\255\255\255\255\255\255\255!"
        "!\255\255\255\255\255\255\255"
        |}]
    ;;

    let%expect_test "signed Fills don't check range" =
      tests
        (List.map
           Fill.
             [ int8_trunc
             ; int16_be_trunc
             ; int16_le_trunc
             ; int32_be_trunc
             ; int32_le_trunc
             ]
           ~f:(fun fill () ->
             fill t ((1 lsl 32) lor Char.to_int '!');
             Iobuf.flip_lo t));
      [%expect
        {|
        "!"
        "\000!"
        "!\000"
        "\000\000\000!"
        "!\000\000\000"
        |}]
    ;;
  end

  let%test_unit _ =
    List.iter [ 0; 1 ] ~f:(fun pos ->
      let t = create ~len:10 in
      if arch_sixtyfour
      then (
        let i = large_int 0x1234 0x5678 0x90AB 0xCDEF in
        (sub_shared t ~pos |> fun t -> Fill.int64_le t i);
        assert (i = (sub_shared t ~pos |> Consume.int64_le_exn)));
      let i = 0x1234_5678 in
      (sub_shared t ~pos |> fun t -> Fill.int32_le_trunc t i);
      assert (i = (sub_shared t ~pos |> Consume.int32_le));
      let i = 0x1234 in
      (sub_shared t ~pos |> fun t -> Fill.int16_le_trunc t i);
      assert (i = (sub_shared t ~pos |> Consume.int16_le));
      ())
  ;;

  let%test_unit _ =
    List.iter strings ~f:(fun s ->
      let t = of_string s in
      let s' = Consume.stringo t in
      assert (is_empty t);
      assert (String.equal s s'))
  ;;

  (* [Fill.string] ranges *)
  let%test_unit _ =
    let s = "hello" in
    let len = String.length s in
    let t = create ~len in
    let try_write ?str_pos ?len () : _ Or_error.t =
      try_with (fun () -> sub_shared t |> fun t -> Fill.stringo t s ?str_pos ?len)
    in
    if is_safe
    then (
      assert (is_error (try_write ~str_pos:(-1) ()));
      assert (is_error (try_write ~str_pos:(len + 1) ()));
      assert (is_error (try_write ~str_pos:0 ~len:(len + 1) ())))
  ;;

  module _ = struct
    open Blit_consume_and_fill

    let blit = blit
    let blito = blito
    let unsafe_blit = unsafe_blit
    let blit_maximal = blit_maximal

    let blit_via_blito ~src ~dst ~len =
      blito () ~src ~dst ?src_len:(if len = length src then None else Some len)
    ;;

    let%test_unit _ =
      List.iter [ blit; unsafe_blit; blit_via_blito ] ~f:(fun blit ->
        let t1 = create ~len:100 in
        let t2 = create ~len:100 in
        let s = "585038473775210758" in
        Fill.stringo t1 s;
        flip_lo t1;
        assert (String.equal s (Consume.stringo t1));
        assert (is_empty t1);
        reset t1;
        Fill.stringo t1 s;
        flip_lo t1;
        blit ~src:t1 ~dst:t2 ~len:(length t1);
        flip_lo t2;
        assert (String.equal s (Consume.stringo t2));
        assert (is_empty t2))
    ;;

    let%expect_test "blit_maximal" =
      let long = of_string (String.make 10 '.') in
      let short = of_string "ABCD" in
      require_equal (module Int) 4 (blit_maximal ~src:short ~dst:long);
      require_equal (module Int) 0 (blit_maximal ~src:short ~dst:long);
      Iobuf.reset short;
      print_s [%sexp (long : (_, _) Iobuf.Limits.Hexdump.Pretty.t)];
      [%expect {| ABCD...... |}];
      require_equal (module Int) 4 (blit_maximal ~src:short ~dst:long);
      Iobuf.reset short;
      print_s [%sexp (long : (_, _) Iobuf.Limits.Hexdump.Pretty.t)];
      [%expect {| ABCDABCD.. |}];
      require_equal (module Int) 2 (blit_maximal ~src:short ~dst:long);
      print_s [%sexp (long : (_, _) Iobuf.Limits.Hexdump.Pretty.t)];
      [%expect {| ABCDABCDAB |}]
    ;;
  end

  module _ = struct
    open Blit_fill

    let blit = blit
    let blito = blito
    let unsafe_blit = unsafe_blit
    let blit_maximal = blit_maximal

    let blit_via_blito ~src ~src_pos ~dst ~len =
      blito
        ()
        ~src
        ?src_pos:(if src_pos = 0 then None else Some src_pos)
        ~dst
        ?src_len:(if len = length src - src_pos then None else Some len)
    ;;

    let%test_unit _ =
      List.iter [ blit; unsafe_blit; blit_via_blito ] ~f:(fun blit ->
        let t1 = of_string "0123456789" in
        let t2 = of_string "qwertyuiop" in
        let t3 = of_string "!@#$%^&*()" in
        [%test_result: string] (to_string t1) ~expect:"0123456789";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
        blit ~dst:t1 ~src:t2 ~src_pos:0 ~len:(length t2);
        [%test_result: string] (to_string t1) ~expect:"";
        reset t1;
        [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
        blit ~dst:t1 ~src:t3 ~src_pos:4 ~len:4;
        [%test_result: string] (to_string t1) ~expect:"tyuiop";
        reset t1;
        [%test_result: string] (to_string t1) ~expect:"%^&*tyuiop";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()")
    ;;

    let%expect_test "blit_maximal" =
      let long = of_string (String.make 10 '.') in
      let short = of_string "ABCD" in
      let t here expected actual =
        require_equal ~here (module Int) expected actual;
        print_s [%sexp (long : (_, _) Iobuf.Limits.Hexdump.Pretty.t)]
      in
      t [%here] 4 (blit_maximal ~src:short ~dst:long ());
      [%expect {| ABCD...... |}];
      t [%here] 3 (blit_maximal ~src:short ~src_pos:1 ~dst:long ());
      [%expect {| ABCDBCD... |}];
      t [%here] 3 (blit_maximal ~src:short ~dst:long ());
      [%expect {| ABCDBCDABC |}]
    ;;
  end

  module _ = struct
    open Blit_consume

    let blit = blit
    let blito = blito
    let unsafe_blit = unsafe_blit
    let blit_maximal = blit_maximal

    let blit_via_blito ~src ~dst ~dst_pos ~len =
      blito
        ()
        ~src
        ~dst
        ?dst_pos:(if dst_pos = 0 then None else Some dst_pos)
        ?src_len:(if len = length src then None else Some len)
    ;;

    let%test_unit _ =
      List.iter [ blit; unsafe_blit; blit_via_blito ] ~f:(fun blit ->
        let t1 = of_string "0123456789" in
        let t2 = of_string "qwertyuiop" in
        let t3 = of_string "!@#$%^&*()" in
        [%test_result: string] (to_string t1) ~expect:"0123456789";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
        blit ~dst:t1 ~dst_pos:0 ~src:t2 ~len:(length t2);
        [%test_result: string] (to_string t2) ~expect:"";
        reset t2;
        [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
        blit ~dst:t1 ~dst_pos:2 ~src:t3 ~len:4;
        [%test_result: string] (to_string t3) ~expect:"%^&*()";
        reset t3;
        [%test_result: string] (to_string t1) ~expect:"qw!@#$uiop";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()")
    ;;

    let sub = sub
    let subo = subo

    let sub_via_subo src ~len =
      subo src ?len:(if len = length src then None else Some len)
    ;;

    let%test_unit _ =
      List.iter [ sub; sub_via_subo ] ~f:(fun sub ->
        let t1 = of_string "0123456789" in
        let t2 = sub t1 ~len:(length t1) in
        [%test_result: string] (to_string t1) ~expect:"";
        reset t1;
        let t3 = sub t1 ~len:4 in
        [%test_result: string] (to_string t1) ~expect:"456789";
        reset t1;
        [%test_result: string] (to_string t1) ~expect:"0123456789";
        [%test_result: string] (to_string t2) ~expect:"0123456789";
        [%test_result: string] (to_string t3) ~expect:"0123";
        Poke.stringo t1 ~pos:0 "qwertyuiop";
        Poke.stringo t2 ~pos:0 "!@#$%^&*()";
        Poke.stringo t3 ~pos:0 "asdf";
        [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t2) ~expect:"!@#$%^&*()";
        [%test_result: string] (to_string t3) ~expect:"asdf")
    ;;

    let%expect_test "blit_maximal" =
      let long = of_string "ABCDEFGHIJ" in
      let short = of_string "...." in
      let t here expected actual =
        require_equal ~here (module Int) expected actual;
        print_s
          [%message
            (long : (_, _) Iobuf.Window.Hexdump.Pretty.t)
              (short : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
      in
      t [%here] 4 (blit_maximal ~src:long ~dst:short ());
      [%expect
        {|
        ((long  EFGHIJ)
         (short ABCD))
        |}];
      t [%here] 3 (blit_maximal ~src:long ~dst_pos:1 ~dst:short ());
      [%expect
        {|
        ((long  HIJ)
         (short AEFG))
        |}];
      t [%here] 3 (blit_maximal ~src:long ~dst:short ());
      [%expect
        {|
        ((long  "")
         (short HIJG))
        |}]
    ;;
  end

  module _ = struct
    open Blit

    let blit = blit
    let blito = blito
    let unsafe_blit = unsafe_blit
    let blit_maximal = blit_maximal

    let blit_via_blito ~src ~src_pos ~dst ~dst_pos ~len =
      blito
        ()
        ~src
        ?src_pos:(if src_pos = 0 then None else Some src_pos)
        ~dst
        ?dst_pos:(if dst_pos = 0 then None else Some dst_pos)
        ?src_len:(if len = length src - src_pos then None else Some len)
    ;;

    let%test_unit _ =
      List.iter [ blit; unsafe_blit; blit_via_blito ] ~f:(fun blit ->
        let t1 = of_string "0123456789" in
        let t2 = of_string "qwertyuiop" in
        let t3 = of_string "!@#$%^&*()" in
        [%test_result: string] (to_string t1) ~expect:"0123456789";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
        blit ~dst:t1 ~dst_pos:0 ~src:t2 ~src_pos:0 ~len:(length t2);
        [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
        blit ~dst:t1 ~dst_pos:2 ~src:t3 ~src_pos:4 ~len:4;
        [%test_result: string] (to_string t1) ~expect:"qw%^&*uiop";
        [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()")
    ;;

    let%test_unit _ =
      List.iter [ blit; unsafe_blit; blit_via_blito ] ~f:(fun blit ->
        let s = "01234567899876543210" in
        let t = create ~len:(String.length s) in
        (Iobuf.sub_shared t |> fun t -> Fill.stringo t s);
        blit ~src:t ~dst:t ~src_pos:0 ~dst_pos:10 ~len:10;
        let s2 = to_string t in
        assert (String.equal s2 "01234567890123456789"))
    ;;

    let sub = sub
    let subo = subo

    let sub_via_subo src ~pos ~len =
      subo
        src
        ?pos:(if pos = 0 then None else Some pos)
        ?len:(if len = length src - pos then None else Some len)
    ;;

    let%test_unit _ =
      List.iter [ sub; sub_via_subo ] ~f:(fun sub ->
        let t1 = of_string "0123456789" in
        let t2 = sub t1 ~pos:0 ~len:(length t1) in
        let t3 = sub t1 ~pos:3 ~len:4 in
        [%test_result: string] (to_string t1) ~expect:"0123456789";
        [%test_result: string] (to_string t2) ~expect:"0123456789";
        [%test_result: string] (to_string t3) ~expect:"3456";
        Poke.stringo t1 ~pos:0 "qwertyuiop";
        Poke.stringo t2 ~pos:0 "!@#$%^&*()";
        Poke.stringo t3 ~pos:0 "asdf";
        [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
        [%test_result: string] (to_string t2) ~expect:"!@#$%^&*()";
        [%test_result: string] (to_string t3) ~expect:"asdf")
    ;;

    let%expect_test "blit_maximal" =
      let long = of_string (String.make 10 '.') in
      let short = of_string "ABCD" in
      let t here expected actual =
        require_equal ~here (module Int) expected actual;
        print_s
          [%message
            (long : (_, _) Iobuf.Window.Hexdump.Pretty.t)
              (short : (_, _) Iobuf.Window.Hexdump.Pretty.t)]
      in
      t [%here] 4 (blit_maximal ~src:short ~dst:long ());
      [%expect
        {|
        ((long  ABCD......)
         (short ABCD))
        |}];
      t [%here] 3 (blit_maximal ~src:short ~dst_pos:7 ~dst:long ());
      [%expect
        {|
        ((long  ABCD...ABC)
         (short ABCD))
        |}];
      t [%here] 1 (blit_maximal ~src:short ~src_pos:3 ~dst:long ());
      [%expect
        {|
        ((long  DBCD...ABC)
         (short ABCD))
        |}]
    ;;
  end

  module _ = struct
    let buf = Expert.buf
    let reinitialize_of_bigstring = Expert.reinitialize_of_bigstring
    let protect_window = Expert.protect_window
    let protect_window_1 = Expert.protect_window_1
    let protect_window_2 = Expert.protect_window_2
    let protect_window_local = Expert.protect_window__local

    let%test_unit "reinitialize_of_bigstring" =
      let iobuf = Iobuf.of_string "1234" in
      let bstr0 = buf iobuf in
      [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "1234");
      let bstr1 = Bigstring.of_string "abcd" in
      reinitialize_of_bigstring iobuf bstr1 ~len:(Bigstring.length bstr1) ~pos:0;
      assert (phys_equal bstr1 (buf iobuf));
      Iobuf.Poke.char iobuf ~pos:0 'A';
      [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "1234");
      [%test_result: Bigstring.t] bstr1 ~expect:(Bigstring.of_string "Abcd");
      [%test_result: string] (Iobuf.to_string iobuf) ~expect:"Abcd"
    ;;

    let%test "set_bounds_and_buffer from ro" =
      let src = read_only (of_string "123abcDEF") in
      let dst = Iobuf.create ~len:0 in
      Expert.set_bounds_and_buffer ~src ~dst;
      src = dst
    ;;

    let%expect_test "protect window" =
      let buf =
        of_string "in-nothing in-long-window in-short-window in-long-window in-nothing"
      in
      Iobuf.advance buf 11;
      Iobuf.resize buf ~len:(Iobuf.length buf - 11);
      print_s [%sexp (buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
      [%expect {| "in-long-window in-short-window in-long-window" |}];
      let buf = no_seek buf in
      protect_window buf ~f:(fun buf ->
        Iobuf.advance buf 15;
        Iobuf.resize buf ~len:(Iobuf.length buf - 15);
        print_s [%sexp (Iobuf.sub_shared buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
        [%expect {| in-short-window |}]);
      print_s [%sexp (buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
      [%expect {| "in-long-window in-short-window in-long-window" |}];
      protect_window_1 buf () ~f:(fun buf () ->
        Iobuf.advance buf 15;
        Iobuf.resize buf ~len:(Iobuf.length buf - 15);
        print_s [%sexp (Iobuf.sub_shared buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
        [%expect {| in-short-window |}]);
      print_s [%sexp (buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
      [%expect {| "in-long-window in-short-window in-long-window" |}];
      protect_window_2 buf () () ~f:(fun buf () () ->
        Iobuf.advance buf 15;
        Iobuf.resize buf ~len:(Iobuf.length buf - 15);
        print_s [%sexp (Iobuf.sub_shared buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
        [%expect {| in-short-window |}]);
      print_s [%sexp (buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
      [%expect {| "in-long-window in-short-window in-long-window" |}];
      protect_window_local buf ~f:(fun buf ->
        Iobuf.advance buf 15;
        Iobuf.resize buf ~len:(Iobuf.length buf - 15);
        print_s [%sexp (Iobuf.globalize0 buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
        [%expect {| in-short-window |}]);
      print_s [%sexp (buf : (_, _) Iobuf.Window.Hexdump.Pretty.t)];
      [%expect {| "in-long-window in-short-window in-long-window" |}]
    ;;
  end

  let%test_unit _ =
    List.iter [ 0; 1 ] ~f:(fun pos ->
      let t = create ~len:10 in
      let i = 0xF234_5678_90AB_CDEFL in
      Poke.int64_t_le t ~pos i;
      assert (Peek.int64_t_le t ~pos = i);
      Poke.int64_t_be t ~pos i;
      assert (Peek.int64_t_be t ~pos = i);
      if arch_sixtyfour
      then (
        let i = large_int 0x1234 0x5678 0x90AB 0xCDEF in
        Poke.int64_t_le t ~pos (Int64.of_int i);
        assert (Peek.int64_t_le t ~pos = Int64.of_int i);
        Poke.int64_t_be t ~pos (Int64.of_int i);
        assert (Peek.int64_t_be t ~pos = Int64.of_int i);
        Poke.int64_le t ~pos i;
        assert (Peek.int64_le_exn t ~pos = i);
        assert (Peek.int64_le_trunc t ~pos = i);
        Poke.int64_be t ~pos i;
        assert (Peek.int64_be_exn t ~pos = i);
        assert (Peek.int64_be_trunc t ~pos = i));
      let i = 0x1234_5678 in
      Poke.int32_le_trunc t ~pos i;
      assert (Peek.int32_le t ~pos = i);
      Poke.int32_be_trunc t ~pos i;
      assert (Peek.int32_be t ~pos = i);
      Poke.uint32_le_trunc t ~pos i;
      assert (Peek.uint32_le t ~pos = i);
      Poke.uint32_be_trunc t ~pos i;
      assert (Peek.uint32_be t ~pos = i);
      let i = 0x1234 in
      Poke.int16_le_trunc t ~pos i;
      assert (Peek.int16_le t ~pos = i);
      Poke.int16_be_trunc t ~pos i;
      assert (Peek.int16_be t ~pos = i);
      Poke.uint16_le_trunc t ~pos i;
      assert (Peek.uint16_le t ~pos = i);
      Poke.uint16_be_trunc t ~pos i;
      assert (Peek.uint16_be t ~pos = i);
      let i = 0x12 in
      Poke.int8_trunc t ~pos i;
      assert (Peek.int8 t ~pos = i);
      Poke.uint8_trunc t ~pos i;
      assert (Peek.uint8 t ~pos = i))
  ;;

  let%test_unit _ =
    let t = create ~len:1024 in
    Fill.int8_trunc t 12;
    Fill.int16_le_trunc t 1234;
    Fill.int32_le_trunc t 345678;
    Fill.char t 'x';
    flip_lo t;
    assert (length t = 8);
    assert (Consume.int8 t = 12);
    assert (Consume.int16_le t = 1234);
    assert (Consume.int32_le t = 345678);
    assert (Consume.char t = 'x')
  ;;
end

let%test_unit _ =
  List.iter [ 0; 1 ] ~f:(fun pos ->
    let t = create ~len:10 in
    let i = 0xF234_5678_90AB_CDEFL in
    Poke.int64_t_le t ~pos i;
    assert (Peek.int64_t_le t ~pos = i);
    Poke.int64_t_be t ~pos i;
    assert (Peek.int64_t_be t ~pos = i);
    if arch_sixtyfour
    then (
      let i = large_int 0x1234 0x5678 0x90AB 0xCDEF in
      Poke.int64_t_le t ~pos (Int64.of_int i);
      assert (Peek.int64_t_le t ~pos = Int64.of_int i);
      Poke.int64_t_be t ~pos (Int64.of_int i);
      assert (Peek.int64_t_be t ~pos = Int64.of_int i);
      Poke.int64_le t ~pos i;
      assert (Peek.int64_le_exn t ~pos = i);
      assert (Peek.int64_le_trunc t ~pos = i);
      Poke.int64_be t ~pos i;
      assert (Peek.int64_be_exn t ~pos = i);
      assert (Peek.int64_be_trunc t ~pos = i));
    let i = 0x1234_5678 in
    Poke.int32_le_trunc t ~pos i;
    assert (Peek.int32_le t ~pos = i);
    Poke.int32_be_trunc t ~pos i;
    assert (Peek.int32_be t ~pos = i);
    Poke.uint32_le_trunc t ~pos i;
    assert (Peek.uint32_le t ~pos = i);
    Poke.uint32_be_trunc t ~pos i;
    assert (Peek.uint32_be t ~pos = i);
    let i = 0x1234 in
    Poke.int16_le_trunc t ~pos i;
    assert (Peek.int16_le t ~pos = i);
    Poke.int16_be_trunc t ~pos i;
    assert (Peek.int16_be t ~pos = i);
    Poke.uint16_le_trunc t ~pos i;
    assert (Peek.uint16_le t ~pos = i);
    Poke.uint16_be_trunc t ~pos i;
    assert (Peek.uint16_be t ~pos = i);
    let i = 0x12 in
    Poke.int8_trunc t ~pos i;
    assert (Peek.int8 t ~pos = i);
    Poke.uint8_trunc t ~pos i;
    assert (Peek.uint8 t ~pos = i))
;;

let%test_unit _ =
  let t = create ~len:1024 in
  Fill.int8_trunc t 12;
  Fill.int16_le_trunc t 1234;
  Fill.int32_le_trunc t 345678;
  Fill.char t 'x';
  flip_lo t;
  assert (length t = 8);
  assert (Consume.int8 t = 12);
  assert (Consume.int16_le t = 1234);
  assert (Consume.int32_le t = 345678);
  assert (Consume.char t = 'x')
;;

include Accessors (struct
    module Consume = Consume
    module Fill = Fill
    module Peek = Peek
    module Poke = Poke

    let is_safe = true
  end)

module _ = Accessors (struct
    include Unsafe

    module Peek = struct
      include Peek

      let assert_equal ~index_or_neg ~index_option t ?pos ?len char =
        let index_or_neg = index_or_neg t ?pos ?len char in
        let index_option = index_option t ?pos ?len char in
        (match index_option with
         | None -> assert (index_or_neg < 0)
         | Some index ->
           assert (index >= 0);
           assert (index = index_or_neg));
        [%globalize: int option] index_option [@nontail]
      ;;

      let index t ?pos ?len char =
        assert_equal ~index_or_neg ~index_option:index__local t ?pos ?len char
      ;;

      let rindex t ?pos ?len char =
        assert_equal
          ~index_or_neg:rindex_or_neg
          ~index_option:rindex__local
          t
          ?pos
          ?len
          char
      ;;
    end

    let is_safe = false
  end)

let memcmp = memcmp
let%test_unit _ = assert (memcmp (Iobuf.create ~len:0) (Iobuf.create ~len:0) = 0)
let%test_unit _ = assert (memcmp (Iobuf.create ~len:0) (Iobuf.create ~len:1) = -1)
let%test_unit _ = assert (memcmp (Iobuf.create ~len:1) (Iobuf.create ~len:0) = 1)

let%test_unit _ =
  let a = Iobuf.of_string "abc" in
  assert (memcmp a a = 0)
;;

let%test_unit _ =
  let a = Iobuf.of_string "abc" in
  let b = Iobuf.of_string "abd" in
  assert (memcmp a b = -1)
;;

module%test [@name "allocation"] _ = struct
  let%expect_test "set_bounds_and_buffer_sub" =
    let src = Iobuf.of_string "123abcDEF" in
    let dst = Iobuf.create ~len:0 in
    require_no_allocation (fun () ->
      Iobuf.set_bounds_and_buffer_sub ~src ~dst ~len:(Iobuf.length src) ~pos:0);
    [%expect {| |}]
  ;;

  let%expect_test "Fill.string" =
    let str = "123" in
    (* The labeled arguments mustn't be constant or they don't test anything - optional
         constant arguments can always be preallocated. *)
    let str_pos = Random.int (String.length str) in
    let len = Random.int (String.length str - str_pos) in
    let dst = Iobuf.create ~len in
    require_no_allocation (fun () -> Iobuf.Fill.string dst str ~str_pos ~len);
    [%expect {| |}]
  ;;

  let%expect_test "of_bigstring_local" =
    let bigstring = Bigstring.of_string "abcdefghijklmnopqrstuvwxyz" in
    require_no_allocation (fun () ->
      let buf = (Iobuf.of_bigstring [@alloc stack]) bigstring in
      ignore (Sys.opaque_identity buf : _ Iobuf.t));
    require_no_allocation (fun () ->
      let buf = (Iobuf.of_bigstring [@alloc stack]) ~pos:1 ~len:10 bigstring in
      ignore (Sys.opaque_identity buf : _ Iobuf.t));
    [%expect {| |}]
  ;;

  let%expect_test "of_string_local" =
    let string = "abc" in
    let buf = (Iobuf.of_string [@alloc stack]) string in
    [%test_eq: string] (Iobuf.to_string buf) string;
    (* 7 words for the Bigstring header + 1 word (regardless of whether 32-bit or 64-bit)
       for the string payload (which is allocated with [malloc] but still tracked as
       custom block out-of-heap resources). *)
    require_allocation_does_not_exceed (Minor_words 8) (fun () ->
      let buf = (Iobuf.of_string [@alloc stack]) string in
      ignore (Sys.opaque_identity buf : _ Iobuf.t));
    [%expect {| |}]
  ;;

  let%expect_test "sub_shared_local" =
    let buf = Iobuf.of_string "abcdefghijklmnopqrstuvwxyz" in
    require_no_allocation (fun () ->
      let buf' = Iobuf.sub_shared__local buf in
      ignore (Sys.opaque_identity buf' : _ Iobuf.t));
    require_no_allocation (fun () ->
      let buf' = Iobuf.sub_shared__local ~pos:1 ~len:5 buf in
      ignore (Sys.opaque_identity buf' : _ Iobuf.t));
    [%expect {| |}]
  ;;

  module Test_read_accessors
      (I : Accessors_read)
      (Apply : sig
         type w

         val apply : ('a, read, w) I.t -> (read, Iobuf.seek) Iobuf.t -> 'a
         val apply_local : ('a, read, w) I.t__local -> (read, Iobuf.seek) Iobuf.t -> 'a
       end) : Accessors_read = struct
    open I

    type nonrec ('a, 'd, 'w) t = ('a, 'd, 'w) t
    type nonrec ('a, 'd, 'w) t__local = ('a, 'd, 'w) t__local

    let test ?(allocation_limit = 0) (f : ('a, 'd, 'w) t) len =
      let buf = Iobuf.of_bigstring (Bigstring.init len ~f:(const '\000')) in
      require_allocation_does_not_exceed (Minor_words allocation_limit) (fun () ->
        ignore (Apply.apply f buf : _))
    ;;

    let test_local (f : ('a, 'd, 'w) t__local) len =
      let buf = Iobuf.of_bigstring (Bigstring.init len ~f:(const '\000')) in
      require_no_allocation (fun () -> ignore (Apply.apply_local f buf : _))
    ;;

    let test_padded_string (f : padding:char -> len:int -> ('a, 'd, 'w) t__local) =
      let pad_amt = 5 in
      let full_len = 20 in
      let padding = ' ' in
      let buf =
        Iobuf.of_bigstring
          (Bigstring.init full_len ~f:(fun i ->
             if pad_amt <= i && i < full_len - pad_amt then 'x' else padding))
      in
      let f = f ~padding ~len:full_len in
      require_no_allocation (fun () -> ignore (Apply.apply_local f buf : _))
    ;;

    let test_string (f : str_pos:int -> len:int -> ('a, 'd, 'w) t__local) =
      let len = 10 in
      test_local (f ~str_pos:0 ~len) len
    ;;

    let test_stringo (f : ?str_pos:int -> ?len:int -> ('a, 'd, 'w) t__local) =
      let len = 10 in
      let buf = Iobuf.of_bigstring (Bigstring.init len ~f:(const ' ')) in
      require_no_allocation (fun () ->
        ignore (Apply.apply_local (f ~str_pos:0 ~len) buf : _))
    ;;

    let char = char

    let%expect_test "char" =
      test char 1;
      [%expect {| |}]
    ;;

    let int8 = int8

    let%expect_test "int8" =
      test int8 1;
      [%expect {| |}]
    ;;

    let int16_be = int16_be

    let%expect_test "int16_be" =
      test int16_be 2;
      [%expect {| |}]
    ;;

    let int16_le = int16_le

    let%expect_test "int16_le" =
      test int16_le 2;
      [%expect {| |}]
    ;;

    let int32_be = int32_be

    let%expect_test "int32_be" =
      test int32_be 4;
      [%expect {| |}]
    ;;

    let int32_le = int32_le

    let%expect_test "int32_le" =
      test int32_le 4;
      [%expect {| |}]
    ;;

    let int64_be_exn = int64_be_exn

    let%expect_test "int64_be_exn" =
      test int64_be_exn 8;
      [%expect {| |}]
    ;;

    let int64_le_exn = int64_le_exn

    let%expect_test "int64_le_exn" =
      test int64_le_exn 8;
      [%expect {| |}]
    ;;

    let int64_be_trunc = int64_be_trunc

    let%expect_test "int64_be_trunc" =
      test int64_be_trunc 8;
      [%expect {| |}]
    ;;

    let int64_le_trunc = int64_le_trunc

    let%expect_test "int64_le_trunc" =
      test int64_le_trunc 8;
      [%expect {| |}]
    ;;

    let uint8 = uint8

    let%expect_test "uint8" =
      test uint8 1;
      [%expect {| |}]
    ;;

    let uint16_be = uint16_be

    let%expect_test "uint16_be" =
      test uint16_be 2;
      [%expect {| |}]
    ;;

    let uint16_le = uint16_le

    let%expect_test "uint16_le" =
      test uint16_le 2;
      [%expect {| |}]
    ;;

    let uint32_be = uint32_be

    let%expect_test "uint32_be" =
      test uint32_be 4;
      [%expect {| |}]
    ;;

    let uint32_le = uint32_le

    let%expect_test "uint32_le" =
      test uint32_le 4;
      [%expect {| |}]
    ;;

    let uint64_be_exn = uint64_be_exn

    let%expect_test "uint64_be_exn" =
      test uint64_be_exn 8;
      [%expect {| |}]
    ;;

    let uint64_le_exn = uint64_le_exn

    let%expect_test "uint64_le_exn" =
      test uint64_le_exn 8;
      [%expect {| |}]
    ;;

    let int64_t_be = int64_t_be

    let%expect_test "int64_t_be" =
      test ~allocation_limit:3 int64_t_be 8;
      [%expect {| |}]
    ;;

    let int64_t_le = int64_t_le

    let%expect_test "int64_t_le" =
      test ~allocation_limit:3 int64_t_le 8;
      [%expect {| |}]
    ;;

    (* We don't bother testing the allocation of string-like accessors that
         heap-allocate their result. We do test the local equivalents below. *)
    let head_padded_fixed_string = head_padded_fixed_string
    let tail_padded_fixed_string = tail_padded_fixed_string
    let string = string
    let bytes = bytes
    let bigstring = bigstring
    let stringo = stringo
    let byteso = byteso
    let bigstringo = bigstringo

    module Local = struct
      open Local

      let int64_t_be = int64_t_be

      let%expect_test "int64_t_be" =
        test_local int64_t_be 8;
        [%expect {| |}]
      ;;

      let int64_t_le = int64_t_le

      let%expect_test "int64_t_le" =
        test_local int64_t_le 8;
        [%expect {| |}]
      ;;

      let head_padded_fixed_string = head_padded_fixed_string

      let%expect_test "head_padded_fixed_string" =
        test_padded_string head_padded_fixed_string;
        [%expect {| |}]
      ;;

      let tail_padded_fixed_string = tail_padded_fixed_string

      let%expect_test "tail_padded_fixed_string" =
        test_padded_string tail_padded_fixed_string;
        [%expect {| |}]
      ;;

      let string = string

      let%expect_test "string" =
        test_string string;
        [%expect {| |}]
      ;;

      let bytes = bytes

      let%expect_test "bytes" =
        test_string bytes;
        [%expect {| |}]
      ;;

      let stringo = stringo

      let%expect_test "stringo" =
        test_stringo stringo;
        [%expect {| |}]
      ;;

      let byteso = byteso

      let%expect_test "byteso" =
        test_stringo byteso;
        [%expect {| |}]
      ;;
    end

    module Int_repr = struct
      open Int_repr

      let int8 = int8

      let%expect_test "int8" =
        test int8 1;
        [%expect {| |}]
      ;;

      let int16_be = int16_be

      let%expect_test "int16_be" =
        test int16_be 2;
        [%expect {| |}]
      ;;

      let int16_le = int16_le

      let%expect_test "int16_le" =
        test int16_le 2;
        [%expect {| |}]
      ;;

      let int32_be = int32_be

      let%expect_test "int32_be" =
        test ~allocation_limit:3 int32_be 4;
        [%expect {| |}]
      ;;

      let int32_le = int32_le

      let%expect_test "int32_le" =
        test ~allocation_limit:3 int32_le 4;
        [%expect {| |}]
      ;;

      let int64_be = int64_be

      let%expect_test "int64_be" =
        test ~allocation_limit:3 int64_be 8;
        [%expect {| |}]
      ;;

      let int64_le = int64_le

      let%expect_test "int64_le" =
        test ~allocation_limit:3 int64_le 8;
        [%expect {| |}]
      ;;

      let uint8 = uint8

      let%expect_test "uint8" =
        test uint8 1;
        [%expect {| |}]
      ;;

      let uint16_be = uint16_be

      let%expect_test "uint16_be" =
        test uint16_be 2;
        [%expect {| |}]
      ;;

      let uint16_le = uint16_le

      let%expect_test "uint16_le" =
        test uint16_le 2;
        [%expect {| |}]
      ;;

      let uint32_be = uint32_be

      let%expect_test "uint32_be" =
        test ~allocation_limit:3 uint32_be 4;
        [%expect {| |}]
      ;;

      let uint32_le = uint32_le

      let%expect_test "uint32_le" =
        test ~allocation_limit:3 uint32_le 4;
        [%expect {| |}]
      ;;

      let uint64_be = uint64_be

      let%expect_test "uint64_be" =
        test ~allocation_limit:3 uint64_be 8;
        [%expect {| |}]
      ;;

      let uint64_le = uint64_le

      let%expect_test "uint64_le" =
        test ~allocation_limit:3 uint64_le 8;
        [%expect {| |}]
      ;;
    end
  end

  module _ =
    Test_read_accessors
      (Iobuf.Peek)
      (struct
        type w = Iobuf.no_seek

        let apply f buf = f (Iobuf.no_seek buf) ~pos:0
        let apply_local f buf = f (Iobuf.no_seek buf) ~pos:0
      end)

  module _ =
    Test_read_accessors
      (Iobuf.Consume)
      (struct
        type w = Iobuf.seek

        let apply f buf = f buf
        let apply_local f buf = f buf
      end)

  module _ =
    Test_read_accessors
      (Iobuf.Unsafe.Peek)
      (struct
        type w = Iobuf.no_seek

        let apply f buf = f (Iobuf.no_seek buf) ~pos:0
        let apply_local f buf = f (Iobuf.no_seek buf) ~pos:0
      end)

  module _ =
    Test_read_accessors
      (Iobuf.Unsafe.Consume)
      (struct
        type w = Iobuf.seek

        let apply f buf = f buf
        let apply_local f buf = f buf
      end)
end

let%test_unit "SEGV bug repro" =
  let tmp = Iobuf.create ~len:10000000 in
  Iobuf.advance tmp (Iobuf.length tmp - 5);
  Iobuf.protect_window_bounds_and_buffer tmp ~f:(fun tmp ->
    let tmp2 = Iobuf.create ~len:5 in
    Iobuf.set_bounds_and_buffer ~src:tmp2 ~dst:tmp);
  ignore (Iobuf.Consume.To_bytes.subo (Iobuf.read_only tmp) : Bytes.t)
;;

let%expect_test "memset" =
  let t = Iobuf.create ~len:32 in
  let pr () = print_s [%sexp (t : (_, _) Iobuf.Window.Hexdump.t)] in
  Iobuf.memset t ~pos:0 ~len:32 'A';
  pr ();
  [%expect
    {|
    ("00000000  41 41 41 41 41 41 41 41  41 41 41 41 41 41 41 41  |AAAAAAAAAAAAAAAA|"
     "00000010  41 41 41 41 41 41 41 41  41 41 41 41 41 41 41 41  |AAAAAAAAAAAAAAAA|")
    |}];
  Iobuf.memset t ~pos:8 ~len:16 'B';
  pr ();
  [%expect
    {|
    ("00000000  41 41 41 41 41 41 41 41  42 42 42 42 42 42 42 42  |AAAAAAAABBBBBBBB|"
     "00000010  42 42 42 42 42 42 42 42  41 41 41 41 41 41 41 41  |BBBBBBBBAAAAAAAA|")
    |}];
  Iobuf.memset t ~pos:2 ~len:3 'C';
  pr ();
  [%expect
    {|
    ("00000000  41 41 43 43 43 41 41 41  42 42 42 42 42 42 42 42  |AACCCAAABBBBBBBB|"
     "00000010  42 42 42 42 42 42 42 42  41 41 41 41 41 41 41 41  |BBBBBBBBAAAAAAAA|")
    |}]
;;

let%expect_test "check seek-indifferent r/w interfaces work on both kinds" =
  let t : (read_write, seek) Iobuf.t = Iobuf.create ~len:8 in
  let c = Iobuf.Peek.char t ~pos:0 in
  let c' = Iobuf.Peek.char (Iobuf.no_seek t) ~pos:0 in
  require_equal (module Char) c c';
  let src = Iobuf.create ~len:8 in
  Iobuf.Blit.blito ~dst:t ~src ();
  Iobuf.Blit.blito ~dst:(Iobuf.no_seek t) ~src ();
  [%expect {| |}]
;;

let%expect_test "concat" =
  let test arr = print_string (Iobuf.to_string (Iobuf.concat arr)) in
  test [||];
  [%expect {| |}];
  test [| Iobuf.of_string "foo" |];
  [%expect {| foo |}];
  test [| Iobuf.of_string "foo"; Iobuf.of_string "bar" |];
  [%expect {| foobar |}];
  let long = Iobuf.of_string "PREFIXfooFILLERbarSUFFIX" in
  test [| Iobuf.sub_shared ~pos:6 ~len:3 long; Iobuf.sub_shared ~pos:15 ~len:3 long |];
  [%expect {| foobar |}]
;;

let%expect_test "transfer" =
  let src = Iobuf.of_string "holy guacamole!" in
  let dst = Iobuf.create ~len:20 in
  Iobuf.transfer ~src ~dst;
  let dump t = [%sexp (t : (_, _) Iobuf.Window.Hexdump.t)] in
  let src, dst = dump src, dump dst in
  require_equal (module Sexp) src dst;
  print_s [%sexp (dst : Sexp.t)];
  [%expect
    {| ("00000000  68 6f 6c 79 20 67 75 61  63 61 6d 6f 6c 65 21     |holy guacamole!|") |}]
;;

let%bench_fun ("blit x100" [@indexed blit_size = [ 1; 32; 100; 1600 ]]) =
  let src = Iobuf.create ~len:blit_size in
  let dst = Iobuf.create ~len:blit_size in
  fun () ->
    Iobuf.reset dst;
    for _ = 1 to 100 do
      Iobuf.Blit.blit ~src ~src_pos:0 ~dst ~len:blit_size ~dst_pos:0
    done
;;

let%bench_fun ("unsafe_blit x100" [@indexed blit_size = [ 1; 32; 100; 1600 ]]) =
  let src = Iobuf.create ~len:blit_size in
  let dst = Iobuf.create ~len:blit_size in
  fun () ->
    Iobuf.reset dst;
    for _ = 1 to 100 do
      Iobuf.Blit.unsafe_blit ~src ~src_pos:0 ~dst ~len:blit_size ~dst_pos:0
    done
;;

let%bench_fun ("blit fill x100" [@indexed blit_size = [ 1; 32; 100; 1600 ]]) =
  let total = blit_size * 100 in
  let src = Iobuf.create ~len:blit_size in
  let dst = Iobuf.create ~len:total in
  fun () ->
    Iobuf.reset dst;
    for _ = 1 to 100 do
      Iobuf.Blit_fill.blit ~src ~src_pos:0 ~dst ~len:blit_size
    done
;;

let%bench_fun ("unsafe_blit fill x100" [@indexed blit_size = [ 1; 32; 100; 1600 ]]) =
  let total = blit_size * 100 in
  let src = Iobuf.create ~len:blit_size in
  let dst = Iobuf.create ~len:total in
  fun () ->
    Iobuf.reset dst;
    for _ = 1 to 100 do
      Iobuf.Blit_fill.unsafe_blit ~src ~src_pos:0 ~dst ~len:blit_size
    done
;;

let%bench_fun ("blit consume x100" [@indexed blit_size = [ 1; 32; 100; 1600 ]]) =
  let total = blit_size * 100 in
  let src = Iobuf.create ~len:total in
  let dst = Iobuf.create ~len:blit_size in
  fun () ->
    Iobuf.reset src;
    for _ = 1 to 100 do
      Iobuf.Blit_consume.blit ~src ~dst ~len:blit_size ~dst_pos:0
    done
;;

let%bench_fun ("unsafe_blit consume x100" [@indexed blit_size = [ 1; 32; 100; 1600 ]]) =
  let total = blit_size * 100 in
  let src = Iobuf.create ~len:total in
  let dst = Iobuf.create ~len:blit_size in
  fun () ->
    Iobuf.reset src;
    for _ = 1 to 100 do
      Iobuf.Blit_consume.unsafe_blit ~src ~dst ~len:blit_size ~dst_pos:0
    done
;;

let%bench_fun ("blit consume+fill x100" [@indexed blit_size = [ 1; 32; 100; 1600 ]]) =
  let total = blit_size * 100 in
  let src = Iobuf.create ~len:total in
  let dst = Iobuf.create ~len:total in
  fun () ->
    Iobuf.reset src;
    Iobuf.reset dst;
    for _ = 1 to 100 do
      Iobuf.Blit_consume_and_fill.blit ~src ~dst ~len:blit_size
    done
;;

let%bench_fun ("unsafe_blit consume+fill x100"
  [@indexed blit_size = [ 1; 32; 100; 1600 ]])
  =
  let total = blit_size * 100 in
  let src = Iobuf.create ~len:total in
  let dst = Iobuf.create ~len:total in
  fun () ->
    Iobuf.reset src;
    Iobuf.reset dst;
    for _ = 1 to 100 do
      Iobuf.Blit_consume_and_fill.unsafe_blit ~src ~dst ~len:blit_size
    done
;;
