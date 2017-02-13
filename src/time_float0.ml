open! Import
open Std_internal

type underlying = Float.t
include
  (Float : sig
     type t = float [@@deriving bin_io, hash, typerep]
     include Comparable.S_common with type t := t
     include module type of struct include Float.O end
   end)

module Span  = Span
module Ofday = (Ofday : Ofday_intf.Ofday with type underlying = underlying and module Span := Span)

(* due to precision limitations in float we can't expect better than microsecond
   precision *)
include Float.Robust_compare.Make
    (struct let robust_comparison_tolerance = 1E-6 end)

let diff t1 t2 = Span.of_sec (t1 - t2)

let add t span = t +. (Span.to_sec span)
let sub t span = t -. (Span.to_sec span)

let prev t = Float.one_ulp `Down t
let next t = Float.one_ulp `Up t

let to_span_since_epoch = Span.of_sec
let of_span_since_epoch = Span.to_sec

(* this is a recreation of the algorithm used internally by the linux kernel
   (supposedly invented by Gauss).  In this case it is used to produce the number
   of seconds since 1970-01-01 00:00:00 using epoch time semantics (86,400 seconds
   per day) *)
let utc_mktime date ofday =
  let open Int.O in
  let year = Date0.year date in
  let month = Month.to_int (Date0.month date) in
  let day = Date0.day date in
  (* move February to the conceptual end of the ordering - 1..12 -> 11,12,1..10 -
     because it carries the leap day.  The months are 0 indexed for this calculation,
     so 1 is February. *)
  let shuffle_year_month year month =
    let month = month - 2 in
    if Int.(<=) month 0 then (year - 1, month + 12) else (year, month)
  in
  let year,month = shuffle_year_month year month in
  let days       = year / 4 - year / 100 + year / 400 + 367 * month / 12 + day in
  let days       = Float.of_int days +. 365. *. Float.of_int year -. 719499. in
  (days *. 86400. +. Span.to_sec (Ofday.to_span_since_start_of_day ofday))
;;

(* Years out of range for [Date.create_exn]. *)
let [@inline never] assert_in_bounds ~sec_since_epoch =
  (* $ TZ=UTC date --date=@-62167219200
     Sat Jan  1 00:00:00 UTC 0000 *)
  let gmtime_lower_bound = -62_167_219_200. in
  (* $ TZ=UTC date --date=@253402300799
     Fri Dec 31 23:59:59 UTC 9999 *)
  let gmtime_upper_bound = 253_402_300_799. in
  (if Float.(>=) sec_since_epoch (gmtime_upper_bound +. 1.)
   || Float.(<) sec_since_epoch gmtime_lower_bound
   then failwithf "Time.gmtime: out of range (%f)" sec_since_epoch ());
;;

let to_days_since_epoch_and_remainder t =
  assert_in_bounds ~sec_since_epoch:t;
  let sec_per_day            = Int63.of_int 86_400 in
  let open Int63.O in
  let days_from_epoch_approx = Int63.of_float t / sec_per_day in
  let days_from_epoch =
    (* when [t] is negative the integer division that calculated days_from_epoch_approx
       will leave us one day short because it truncates (e.g. -100 / 86_400 = 0 and we
       want -1) -- adjust for that here. *)
    if Float.(<) t (Int63.to_float (days_from_epoch_approx * sec_per_day))
    then Int63.pred days_from_epoch_approx
    else days_from_epoch_approx
  in
  let days_from_epoch_in_sec = Int63.to_float (days_from_epoch * sec_per_day) in
  let remainder = t -. days_from_epoch_in_sec in
  (Int63.to_int_exn days_from_epoch, Span.of_sec remainder)
;;

let now () =
  let float_ns =
    Time_ns.now ()
    |> Time_ns.to_int63_ns_since_epoch
    |> Int63.to_float
  in
  of_span_since_epoch (Span.of_sec (float_ns *. 1E-9))
;;
