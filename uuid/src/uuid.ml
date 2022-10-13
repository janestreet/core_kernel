(* A loose implementation of version 3 of the UUID spec:

   Version 3 UUIDs use a scheme deriving a UUID via MD5 from a URL, a fully
   qualified domain name, an object identifier, a distinguished name (DN as used
   in Lightweight Directory Access Protocol), or on names in unspecified
   namespaces. Version 3 UUIDs have the form xxxxxxxx-xxxx-3xxx-xxxx-xxxxxxxxxxxx
   with hexadecimal digits x.
*)

module Stable = struct
  open Core.Core_stable

  module V1 = struct
    module T = struct
      type t = string [@@deriving bin_io, compare, equal, hash, sexp, stable_witness]

      include (val Comparator.V1.make ~compare ~sexp_of_t)
    end

    include T
    include Comparable.V1.With_stable_witness.Make (T)

    let for_testing = "5a863fc1-67b7-3a0a-dc90-aca2995afbf9"
  end
end

open! Core

module T = struct
  type t = string [@@deriving bin_io, compare, hash]
  type comparator_witness = Stable.V1.comparator_witness

  let comparator = Stable.V1.comparator

  let next_counter =
    let counter = ref 0 in
    fun () ->
      (* In OCaml this doesn't allocate, and threads can't context switch except on
         allocation *)
      incr counter;
      !counter
  ;;

  let set_all_dashes bytes =
    Bytes.set bytes 8 '-';
    Bytes.set bytes 13 '-';
    Bytes.set bytes 18 '-';
    Bytes.set bytes 23 '-'
  ;;

  let set_version bytes ~version = Bytes.set bytes 14 version
  let to_string = Fn.id

  (*{v
     xxxxxxxx-xxxx-3xxx-xxxx-xxxxxxxxxxxx
     xxxxxxxx-xxxx-4xxx-xxxx-xxxxxxxxxxxx
     012345678901234567890123456789012345
     0         1         2         3
  v}*)

  let char_is_dash c = Char.equal '-' c

  let is_valid_exn s =
    (* we don't check for a 3 in the version position (14) because we want to be
       generous about accepting UUIDs generated by other versions of the protocol, and
       we want to be resilient to future changes in this algorithm. *)
    assert (String.length s = 36);
    assert (String.count s ~f:char_is_dash = 4);
    assert (char_is_dash s.[8]);
    assert (char_is_dash s.[13]);
    assert (char_is_dash s.[18]);
    assert (char_is_dash s.[23])
  ;;

  let of_string s =
    try
      is_valid_exn s;
      s
    with
    | _ -> failwithf "%s: not a valid UUID" s ()
  ;;

  let bottom_4_bits_to_hex_char v =
    let v = v land 0x0F in
    if v < 10 then Char.unsafe_of_int (48 + v) else Char.unsafe_of_int (87 + v)
  ;;

  let create_random =
    let bytes = Bytes.create 36 in
    fun random_state ->
      (* We fill all 36 bytes with random hex digits, and then go back and set specific
         bytes to dash and the version number (4).  We do 6 groups of 6 bytes, each time
         using 24 bits of a random int, 4 for each hex digit. *)
      let at = ref 0 in
      for _ = 1 to 6 do
        let int = ref (Random.State.bits random_state) in
        for _ = 1 to 6 do
          Bytes.set bytes !at (bottom_4_bits_to_hex_char !int);
          incr at;
          int := !int lsr 4
        done
      done;
      set_all_dashes bytes;
      set_version bytes ~version:'4';
      Bytes.to_string bytes
  ;;

  (* [create] is responsible for generating unique string identifiers.  It should be clear
     to a reader that the id generated has an extremely high probability of uniqueness
     across all possible machines, processes, and threads of execution. *)

  let create ~hostname ~pid =
    let digest =
      let time = Time_float.now () in
      let counter = next_counter () in
      let base =
        String.concat
          ~sep:"-"
          [ hostname
          ; Int.to_string pid
          ; Float.to_string_12
              (Time_float.Span.to_sec (Time_float.to_span_since_epoch time))
          ; Int.to_string counter
          ]
      in
      Md5.to_hex (Md5.digest_string base)
    in
    let s = Bytes.create 36 in
    set_all_dashes s;
    Bytes.From_string.blit ~src:digest ~dst:s ~src_pos:0 ~dst_pos:0 ~len:8;
    Bytes.From_string.blit ~src:digest ~dst:s ~src_pos:8 ~dst_pos:9 ~len:4;
    Bytes.From_string.blit ~src:digest ~dst:s ~src_pos:12 ~dst_pos:14 ~len:4;
    Bytes.From_string.blit ~src:digest ~dst:s ~src_pos:16 ~dst_pos:19 ~len:4;
    Bytes.From_string.blit ~src:digest ~dst:s ~src_pos:20 ~dst_pos:24 ~len:12;
    set_version s ~version:'3';
    Bytes.to_string s
  ;;
end

include T

include Identifiable.Make_using_comparator (struct
    let module_name = "Uuid"

    include T
    include Sexpable.Of_stringable (T)
  end)

let invariant t = ignore (of_string t : t)
let nil = "00000000-0000-0000-0000-000000000000"

module Unstable = struct
  type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp]
  type nonrec comparator_witness = comparator_witness

  let comparator = comparator
  let t_sexp_grammar = string_sexp_grammar
end

let arg_type = Command.Arg_type.create of_string

let sexp_of_t t =
  if am_running_test then [%sexp "<uuid-omitted-in-test>"] else [%sexp (t : t)]
;;

module Private = struct
  let create = create
  let is_valid_exn = is_valid_exn
  let bottom_4_bits_to_hex_char = bottom_4_bits_to_hex_char
  let nil = nil
end

let quickcheck_shrinker : t Quickcheck.Shrinker.t = Quickcheck.Shrinker.empty ()
let quickcheck_observer : t Quickcheck.Observer.t = Quickcheck.Observer.of_hash (module T)

let quickcheck_generator : t Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let gen_hex_digit : Char.t Quickcheck.Generator.t =
    Quickcheck.Generator.weighted_union
      [ 10.0, Char.gen_digit; 6.0, Char.gen_uniform_inclusive 'a' 'f' ]
  in
  let%map first = String.gen_with_length 8 gen_hex_digit
  and second = String.gen_with_length 4 gen_hex_digit
  and third = String.gen_with_length 4 gen_hex_digit
  and fourth = String.gen_with_length 4 gen_hex_digit
  and fifth = String.gen_with_length 12 gen_hex_digit in
  of_string (sprintf "%s-%s-%s-%s-%s" first second third fourth fifth)
;;
