open! Import
open Std_internal

let arch_sixtyfour = Sys.word_size = 64

module Span = Span_ns
module Ofday = Ofday_ns

type t = Span.t (* since the Unix epoch (1970-01-01 00:00:00 UTC) *)
[@@deriving bin_io, compare, hash, typerep]

include (Span : Comparable.Infix with type t := t)

let now = Span.since_unix_epoch

let equal = Span.equal

let min_value = Span.min_value
let max_value = Span.max_value

let epoch        = Span.zero
let add          = Span.(+)
let sub          = Span.(-)
let diff         = Span.(-)
let abs_diff t u = Span.abs (diff t u)
let max          = Span.max
let min          = Span.min

let to_span_since_epoch t = t
let of_span_since_epoch s = s

let to_int63_ns_since_epoch t : Int63.t = Span.to_int63_ns (to_span_since_epoch t)
let of_int63_ns_since_epoch i = of_span_since_epoch (Span.of_int63_ns i)

let to_int_ns_since_epoch =
  if arch_sixtyfour
  then fun t -> Int63.to_int_exn (to_int63_ns_since_epoch t)
  else fun _ -> failwith "Time_ns.to_int_ns_since_epoch: unsupported on 32bit machines"

let of_int_ns_since_epoch =
  if arch_sixtyfour
  then fun i -> of_int63_ns_since_epoch (Int63.of_int i)
  else fun _ -> failwith "Time_ns.of_int_ns_since_epoch: unsupported on 32bit machines"

let [@inline never] raise_next_multiple_got_nonpositive_interval interval =
  failwiths "Time_ns.next_multiple got nonpositive interval" interval
    [%sexp_of: Span.t]
;;

let next_multiple ?(can_equal_after = false) ~base ~after ~interval () =
  if Span.(<=) interval Span.zero
  then raise_next_multiple_got_nonpositive_interval interval;
  let base_to_after = diff after base in
  if Span.(<) base_to_after Span.zero
  then base  (* [after < base], choose [k = 0]. *)
  else begin
    let next =
      add base (Span.scale_int63 interval (Span.div base_to_after interval))
    in
    if next > after || (can_equal_after && next = after)
    then next
    else add next interval
  end
;;

let random ?state () = Span.random ?state ()

module Utc : sig
  val to_date_and_span_since_start_of_day : t -> Date0.t * Span.t
  val of_date_and_span_since_start_of_day : Date0.t -> Span.t -> t
end = struct
  (* a recreation of the system call gmtime specialized to the fields we need that also
     doesn't rely on Unix. *)
  let to_date_and_span_since_start_of_day t =
    let open Int63.O in
    let (!<) i = Int63.of_int_exn i in
    let (!>) t = Int63.to_int_exn t in
    let ns_since_epoch  = to_int63_ns_since_epoch t   in
    let ns_per_day      = !<86_400 * !<1_000_000_000  in
    let approx_days_from_epoch = ns_since_epoch / ns_per_day in
    let days_from_epoch =
      if ns_since_epoch < !<0 && approx_days_from_epoch * ns_per_day <> ns_since_epoch
      then approx_days_from_epoch - !<1
      else approx_days_from_epoch
    in
    let ns_since_start_of_day = ns_since_epoch - (ns_per_day * days_from_epoch) in
    let date =
      Date0.Days.add_days Date0.Days.unix_epoch !>days_from_epoch
      |> Date0.Days.to_date
    in
    let span_since_start_of_day = Span.of_int63_ns ns_since_start_of_day in
    date, span_since_start_of_day
  ;;

  let of_date_and_span_since_start_of_day date span_since_start_of_day =
    assert (Span.( >= ) span_since_start_of_day Span.zero
            && Span.( < ) span_since_start_of_day Span.day);
    let days_from_epoch =
      Date0.Days.diff (Date0.Days.of_date date) Date0.Days.unix_epoch
    in
    let span_in_days_since_epoch = Span.scale_int Span.day days_from_epoch in
    let span_since_epoch = Span.( + ) span_in_days_since_epoch span_since_start_of_day in
    of_span_since_epoch span_since_epoch
  ;;
end

module Alternate_sexp = struct
  type nonrec t = t

  module Ofday_as_span = struct
    open Int.O

    let seconds_to_string seconds_span =
      let seconds = Span.to_int_sec seconds_span in
      let h = seconds / 3600 in
      let m = (seconds / 60) % 60 in
      let s = seconds % 60 in
      sprintf "%02d:%02d:%02d" h m s

    let two_digit_of_string string =
      assert (String.length string = 2
              && String.for_all string ~f:Char.is_digit);
      Int.of_string string

    let seconds_of_string seconds_string =
      match String.split seconds_string ~on:':' with
      | [ h_string ; m_string ; s_string ] ->
        let h = two_digit_of_string h_string in
        let m = two_digit_of_string m_string in
        let s = two_digit_of_string s_string in
        Span.of_int_sec ((((h * 60) + m) * 60) + s)
      | _ -> assert false

    let ns_of_100_ms = 100_000_000
    let ns_of_10_ms  =  10_000_000
    let ns_of_1_ms   =   1_000_000
    let ns_of_100_us =     100_000
    let ns_of_10_us  =      10_000
    let ns_of_1_us   =       1_000
    let ns_of_100_ns =         100
    let ns_of_10_ns  =          10
    let ns_of_1_ns   =           1

    let sub_second_to_string sub_second_span =
      let open Int.O in
      let ns = Span.to_int63_ns sub_second_span |> Int63.to_int_exn in
      if ns = 0
      then ""
      else begin
        if ns % ns_of_100_ms = 0 then sprintf ".%01d" (ns / ns_of_100_ms) else
        if ns % ns_of_10_ms  = 0 then sprintf ".%02d" (ns / ns_of_10_ms)  else
        if ns % ns_of_1_ms   = 0 then sprintf ".%03d" (ns / ns_of_1_ms)   else
        if ns % ns_of_100_us = 0 then sprintf ".%04d" (ns / ns_of_100_us) else
        if ns % ns_of_10_us  = 0 then sprintf ".%05d" (ns / ns_of_10_us)  else
        if ns % ns_of_1_us   = 0 then sprintf ".%06d" (ns / ns_of_1_us)   else
        if ns % ns_of_100_ns = 0 then sprintf ".%07d" (ns / ns_of_100_ns) else
        if ns % ns_of_10_ns  = 0 then sprintf ".%08d" (ns / ns_of_10_ns)  else
          sprintf ".%09d" ns
      end

    let sub_second_of_string string =
      if String.is_empty string
      then Span.zero
      else begin
        let digits = String.chop_prefix_exn string ~prefix:"." in
        assert (String.for_all digits ~f:Char.is_digit);
        let multiplier =
          match String.length digits with
          | 1 -> ns_of_100_ms
          | 2 -> ns_of_10_ms
          | 3 -> ns_of_1_ms
          | 4 -> ns_of_100_us
          | 5 -> ns_of_10_us
          | 6 -> ns_of_1_us
          | 7 -> ns_of_100_ns
          | 8 -> ns_of_10_ns
          | 9 -> ns_of_1_ns
          | _ -> assert false
        in
        Span.of_int63_ns (Int63.of_int (Int.of_string digits * multiplier))
      end

    let to_string span =
      assert (Span.( >= ) span Span.zero && Span.( < ) span Span.day);
      let seconds_span = span |> Span.to_int_sec |> Span.of_int_sec in
      let sub_second_span = Span.( - ) span seconds_span in
      seconds_to_string seconds_span ^ sub_second_to_string sub_second_span

    let of_string string =
      let len = String.length string in
      let prefix_len = 8 in (* "HH:MM:DD" *)
      let suffix_len = len - prefix_len in
      let seconds_string    = String.sub string ~pos:0          ~len:prefix_len in
      let sub_second_string = String.sub string ~pos:prefix_len ~len:suffix_len in
      let seconds_span    = seconds_of_string    seconds_string    in
      let sub_second_span = sub_second_of_string sub_second_string in
      Span.( + ) seconds_span sub_second_span
  end
  let to_string t =
    let date, span_since_start_of_day = Utc.to_date_and_span_since_start_of_day t in
    Date0.to_string date
    ^ " "
    ^ Ofday_as_span.to_string span_since_start_of_day
    ^ "Z"

  let of_string string =
    let date_string, ofday_string_with_zone =
      String.lsplit2_exn string ~on:' '
    in
    let ofday_string =
      String.chop_suffix_exn ofday_string_with_zone ~suffix:"Z"
    in
    let date = Date0.of_string date_string in
    let ofday = Ofday_as_span.of_string ofday_string in
    Utc.of_date_and_span_since_start_of_day date ofday

  include Sexpable.Of_stringable (struct
      type nonrec t = t
      let to_string = to_string
      let of_string = of_string
    end)

  module Stable = struct
    module V1 = struct
      (* see tests in lib/core_kernel/test/test_time_ns that ensure stability of this
         representation *)
      type nonrec t = t [@@deriving bin_io, compare, sexp]
    end
  end
end

module Stable = struct
  module Alternate_sexp = Alternate_sexp.Stable
  module Span = Span.Stable
  module Ofday = Ofday.Stable
end
