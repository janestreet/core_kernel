INCLUDE "config.mlh"
open Std_internal


let round_nearest_portable_alloc f = Core_int63.of_float (Float.round_nearest f)
let round_nearest_arch64_noalloc f = Core_int63.of_int (Float.iround_nearest_exn f)
BENCH_MODULE "round_nearest portability/performance" = struct
  let f = if Random.bool () then 1.0 else 2.0
  BENCH "round_nearest_portable_alloc" = round_nearest_portable_alloc f
  BENCH "round_nearest_arch64_noalloc" = round_nearest_arch64_noalloc f
  (* Here is a comparison of both of these rounding operators on a 64-bit machine.  Hence
     we have special-cased this so that we get the faster operation on 64-bit machines.

     ┌───────────────────────────────────────────┬──────────┬─────────┬────────────┐
     │ Name                                      │ Time/Run │ mWd/Run │ Percentage │
     ├───────────────────────────────────────────┼──────────┼─────────┼────────────┤
     │ [time_ns.ml] round_nearest_portable_alloc │  17.31ns │   2.00w │    100.00% │
     │ [time_ns.ml] round_nearest_arch64_noalloc │   4.53ns │         │     26.16% │
     └───────────────────────────────────────────┴──────────┴─────────┴────────────┘
  *)
end
IFDEF ARCH_SIXTYFOUR THEN
let round_nearest = round_nearest_arch64_noalloc
ELSE
let round_nearest = round_nearest_portable_alloc
ENDIF

let float x = Core_int63.to_float x

(* We have pulled this up here so that we have a way for formatting times in their sexp
   representation. *)
module Platform_specific = struct
  type tm =
    Unix.tm = {
    (* DON'T CHANGE THIS RECORD WITHOUT UPDATING time_ns_stubs.c!!!

       The compiler will notice if the runtime's Unix.tm changes, and we must then update
       time_ns_stubs.c, not just this copy of the definition. *)
    tm_sec   : int;
    tm_min   : int;
    tm_hour  : int;
    tm_mday  : int;
    tm_mon   : int;
    tm_year  : int;
    tm_wday  : int;
    tm_yday  : int;
    tm_isdst : bool;
  } with sexp

  external strftime : tm -> string -> string = "core_kernel_time_ns_strftime"

  TEST_UNIT "record format hasn't changed" =
    (* Exclude the time zone (%Z) because it depends on the location. *)
    <:test_result< string >> ~expect:"1907-07-05 04:03:08; wday=2; yday=010"
      (strftime
         { tm_sec = 8; tm_min = 3; tm_hour = 4; tm_mday = 5; tm_mon = 6; tm_year = 7;
           tm_wday = 2; tm_yday = 9; tm_isdst = true }
         "%F %T; wday=%u; yday=%j")

  let internal_round_nearest = round_nearest
end

(* This signature constraint is semi-temporary and serves to make the implementation more
   type-safe (so the compiler can help us more).  It would go away if we broke the
   implementation into multiple files. *)
module Span : sig
  (* Note that the [sexp] below is implemented only for some debug text later in this
     module. It is not exposed in the mli. *)
  type t = Core_int63.t with typerep, compare, bin_io, sexp

  include Comparable.Infix     with type t := t
  include Comparable.Validate  with type t := t
  include Comparable.With_zero with type t := t
  include Equal.S              with type t := t

  val min : t -> t -> t

  val nanosecond  : t
  val microsecond : t
  val millisecond : t
  val second      : t
  val minute      : t
  val hour        : t
  val day         : t

  val of_ns  : float -> t
  val of_us  : float -> t
  val of_ms  : float -> t
  val of_sec : float -> t
  val of_min : float -> t
  val of_hr  : float -> t
  val of_day : float -> t
  val to_ns  : t     -> float
  val to_us  : t     -> float
  val to_ms  : t     -> float
  val to_sec : t     -> float
  val to_min : t     -> float
  val to_hr  : t     -> float
  val to_day : t     -> float

  val of_int_sec : int -> t
  val to_int_sec : t -> int

  val of_int63_ns : Core_int63.t -> t
  val to_int63_ns : t -> Core_int63.t
  val of_int_ns : int -> t
  val to_int_ns : t   -> int

  val zero : t
  val min_value : t
  val max_value : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val abs : t -> t
  val neg : t -> t
  val scale     : t -> float -> t
  val scale_int : t -> int   -> t
  val div : t -> t -> int
  val ( / ) : t -> float -> t
  val ( // ) : t -> t -> float

  val create
    :  ?sign : Float.Sign.t
    -> ?day : int
    -> ?hr  : int
    -> ?min : int
    -> ?sec : int
    -> ?ms  : int
    -> ?us  : int
    -> ?ns  : int
    -> unit
    -> t

  module Parts : sig
    type t =
      { sign : Float.Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    with sexp
  end

  val to_parts : t -> Parts.t
  val of_parts : Parts.t -> t

  include Robustly_comparable with type t := t

  val since_unix_epoch : unit -> t

  module Alternate_sexp : sig
    type nonrec t = t with sexp
  end
end = struct
  let half_microsecond = Core_int63.of_int 500

  let min_value = Core_int63.min_value

  (* [- half_microsecond] to avoid overflow in [Core.Std.Time_ns.to_span max_value] *)
  let max_value = Core_int63.(max_value - half_microsecond)

  (* [Span] is basically a [Core_int63] *)
  module T = struct
    type t = Core_int63.t (** nanoseconds *)
    with bin_io, typerep

    let compare = Core_int63.compare
    let equal = Core_int63.equal
    let zero = Core_int63.zero

    include (Core_int63 : Comparable.Infix with type t := t)

    let min = Core_int63.min
  end

  include T

  module Parts = struct
    type t =
      { sign : Float.Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    with sexp

    let compare = Poly.compare
  end

  let nanosecond  = Core_int63.of_int 1
  let microsecond = Core_int63.(of_int 1000 * nanosecond)
  let millisecond = Core_int63.(of_int 1000 * microsecond)
  let second      = Core_int63.(of_int 1000 * millisecond)
  let minute      = Core_int63.(of_int 60 * second)
  let hour        = Core_int63.(of_int 60 * minute)
  let day         = Core_int63.(of_int 24 * hour)

  let create
        ?(sign = Float.Sign.Pos)
        ?day:(d = 0)
        ?(hr  = 0)
        ?(min = 0)
        ?(sec = 0)
        ?(ms  = 0)
        ?(us  = 0)
        ?(ns  = 0)
        () =
    let minutes = min in
    let open Core_int63 in
    let t =
      of_int d * day
      + of_int hr * hour
      + of_int minutes * minute
      + of_int sec * second
      + of_int ms * millisecond
      + of_int us * microsecond
      + of_int ns * nanosecond
    in
    Float.Sign.(match sign with Neg -> -t | Pos | Zero -> t)

  let to_parts t =
    let open Core_int63 in
    let mag = abs t in
    { Parts.
      sign = Float.Sign.(if t < zero then Neg else if t > zero then Pos else Zero)
    ; hr = to_int_exn (mag / hour)
    ; min = to_int_exn ((rem mag hour) / minute)
    ; sec = to_int_exn ((rem mag minute) / second)
    ; ms = to_int_exn ((rem mag second) / millisecond)
    ; us = to_int_exn ((rem mag millisecond) / microsecond)
    ; ns = to_int_exn ((rem mag microsecond) / nanosecond)
    }

  let of_parts { Parts. sign; hr; min; sec; ms; us; ns } =
    create ~sign ~hr ~min ~sec ~ms ~us ~ns ()

  let of_ns       f = round_nearest f
  let of_int63_ns i = i
  let of_int_sec  i = Core_int63.(of_int i * second)
  let of_us       f = round_nearest (f *. float microsecond)
  let of_ms       f = round_nearest (f *. float millisecond)
  let of_sec      f = round_nearest (f *. float second)
  let of_min      f = round_nearest (f *. float minute)
  let of_hr       f = round_nearest (f *. float hour)
  let of_day      f = round_nearest (f *. float day)

  let to_ns       t = float t
  let to_int63_ns t =       t
  let to_us       t = float t /. float microsecond
  let to_ms       t = float t /. float millisecond
  let to_sec      t = float t /. float second
  let to_min      t = float t /. float minute
  let to_hr       t = float t /. float hour
  let to_day      t = float t /. float day
  let to_int_sec  t = Core_int63.(to_int_exn (t / second))
  TEST = Int.(>) (to_int_sec Core_int63.max_value) 0 (* and doesn't raise *)

IFDEF ARCH_SIXTYFOUR THEN
  let of_int_ns i = of_int63_ns (Core_int63.of_int i)
  let to_int_ns t = Core_int63.to_int_exn (to_int63_ns t)
ELSE
  let of_int_ns _i = failwith "unsupported on 32bit machines"
  let to_int_ns _i = failwith "unsupported on 32bit machines"
ENDIF

  let (+)       = Core_int63.(+)
  let (-)       = Core_int63.(-)
  let abs       = Core_int63.(abs)
  let neg       = Core_int63.(neg)
  let scale t f = round_nearest (float t *. f)
  let scale_int t i = Core_int63.(t * of_int i)
  let div t1 t2 = Core_int63.(to_int_exn (t1 /% t2))
  let (/)   t f = round_nearest (float t /. f)
  let (//)      = Core_int63.(//)

  (** The conversion code here is largely copied from [Core.Span] and edited to remove
      some of the stable versioning details. This makes it a little easier to think about
      and we get a compatible sexp format that can subsequently live in [Core_kernel] and
      [Async_kernel] *)
  module Alternate_sexp = struct
    type nonrec t = t

    let of_string (s:string) =
      try
        begin match s with
        | "" -> failwith "empty string"
        | _  ->
          let float n =
            match (String.drop_suffix s n) with
            | "" -> failwith "no number given"
            | s  ->
              let v = Float.of_string s in
              Validate.maybe_raise (Float.validate_ordinary v);
              v
          in
          let len = String.length s in
          match s.[Int.(-) len 1] with
          | 's' ->
            if Int.(>=) len 2 && Char.(=) s.[Int.(-) len 2] 'm'
            then of_ms (float 2)
            else if Int.(>=) len 2 && Char.(=) s.[Int.(-) len 2] 'u'
            then of_us (float 2)
            else if Int.(>=) len 2 && Char.(=) s.[Int.(-) len 2] 'n'
            then of_ns (float 2)
            else of_sec (float 1)
          | 'm' -> of_min (float 1)
          | 'h' -> of_hr (float 1)
          | 'd' -> of_day (float 1)
          | _ ->
            failwith "Time spans must end in ns, us, ms, s, m, h, or d."
        end
      with exn ->
        failwithf "Span.of_string could not parse '%s': %s" s (Exn.to_string exn) ()

    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom x ->
        (try of_string x
        with exn -> of_sexp_error (Exn.to_string exn) sexp)
      | Sexp.List _ ->
        of_sexp_error "Time_ns.Span.t_of_sexp sexp must be an Atom" sexp


    let to_string (t:T.t) =
      let string suffix float =
        (* This is the same float-to-string conversion used in [Float.sexp_of_t].  It's
           like [Float.to_string_round_trippable], but may leave off trailing period. *)
        !Sexplib.Conv.default_string_of_float float ^ suffix
      in
      let abs_t = abs t in
      if abs_t < microsecond then string "ns" (to_ns t)
      else if abs_t < millisecond then string "us" (to_us t)
      else if abs_t < second then string "ms" (to_ms t)
      else if abs_t < minute then string "s" (to_sec t)
      else if abs_t < hour then string "m" (to_min t)
      else if abs_t < day then string "h" (to_hr t)
      else string "d" (to_day t)

    let sexp_of_t t = Sexp.Atom (to_string t)
  end

  let sexp_of_t = Alternate_sexp.sexp_of_t
  let t_of_sexp = Alternate_sexp.t_of_sexp

  include Comparable.Validate_with_zero (struct
      include T
      let sexp_of_t = Alternate_sexp.sexp_of_t
      let t_of_sexp = Alternate_sexp.t_of_sexp
    end)

  TEST_MODULE = struct
    let ( * ) = Core_int63.( * )
    let of_int = Core_int63.of_int

    let round_trip t = <:test_result< t >> (of_parts (to_parts t)) ~expect:t
    let eq t expect =
      <:test_result< t >> t ~expect;
      <:test_result< Parts.t >> (to_parts t) ~expect:(to_parts expect);
      round_trip t

    TEST_UNIT = eq (create ~us:2                       ()) (of_int 2    * microsecond)
    TEST_UNIT = eq (create ~min:3                      ()) (of_int 3    * minute)
    TEST_UNIT = eq (create ~ms:4                       ()) (of_int 4    * millisecond)
    TEST_UNIT = eq (create ~sec:5                      ()) (of_int 5    * second)
    TEST_UNIT = eq (create ~hr:6                       ()) (of_int 6    * hour)
    TEST_UNIT = eq (create ~day:7                      ()) (of_int 7    * day)
    TEST_UNIT = eq (create ~us:8 ~sign:Float.Sign.Neg  ()) (of_int (-8) * microsecond)
    TEST_UNIT = eq (create ~ms:9 ~sign:Float.Sign.Zero ()) (of_int 9    * millisecond)
    TEST_UNIT =
      let open Core_int63 in
      for _i = 1 to 1_000_000 do
        let t =
          (of_int64_exn (Random.int64 (to_int64 max_value)))
          + if Random.bool () then zero else min_value
        in
        round_trip t
      done

    let round_trip parts =
      <:test_result< Parts.t >> (to_parts (of_parts parts)) ~expect:parts
    let eq parts expect =
      <:test_result< Parts.t >> parts ~expect;
      <:test_result< t >> (of_parts parts) ~expect:(of_parts expect);
      round_trip parts

    TEST_UNIT =
      eq (to_parts (create ~sign:Float.Sign.Neg ~hr:2 ~min:3 ~sec:4 ~ms:5 ~us:6 ~ns:7 ()))
        { Parts. sign = Float.Sign.Neg; hr = 2; min = 3; sec = 4; ms = 5; us = 6; ns = 7 }
    TEST_UNIT = round_trip (to_parts (create ~hr:25 ()))
    TEST_UNIT =
      let hr =
        match Word_size.word_size with
        | W32 -> Core_int.max_value
        | W64 -> Core_int64.to_int_exn 2217989799822798757L
      in
      round_trip (to_parts (create ~hr ()))
  end

  (* Functions required by [Robustly_comparable]: allows for [epsilon] granularity.

     A microsecond is a reasonable granularity because there is very little network
     activity that can be measured to sub-microsecond resolution. *)
  let epsilon = microsecond
  let (>=.) t u = t >= Core_int63.(u - epsilon)
  let (<=.) t u = t <= Core_int63.(u + epsilon)
  let (=.) t u = Core_int63.(abs (t - u)) <= epsilon
  let (>.) t u = t > Core_int63.(u + epsilon)
  let (<.) t u = t < Core_int63.(u - epsilon)
  let (<>.) t u = Core_int63.(abs (t - u)) > epsilon
  let robustly_compare t u = if t <. u then -1 else if t >. u then 1 else 0

IFDEF ARCH_SIXTYFOUR THEN
  external since_unix_epoch_or_zero : unit -> t
    = "core_kernel_time_ns_gettime_or_zero" "noalloc"
ELSE
  external since_unix_epoch_or_zero : unit -> t
    = "core_kernel_time_ns_gettime_or_zero"
ENDIF

IFDEF POSIX_TIMERS THEN
  let gettime_failed () = failwith "clock_gettime(CLOCK_REALTIME) failed"
ELSE
  let gettime_failed () = failwith "gettimeofday failed"
ENDIF

  let since_unix_epoch () =
    let t = since_unix_epoch_or_zero () in
    if t <> zero then t else gettime_failed ()
  ;;
end

type t = Span.t (** since the Unix epoch (1970-01-01 00:00:00 UTC) *)
with bin_io, compare, typerep

include (Span : Comparable.Infix with type t := t)

let now = Span.since_unix_epoch

let equal = Span.equal

let min_value = Span.min_value
let max_value = Span.max_value

let epoch = Span.zero
let add = Span.(+)
let sub = Span.(-)
let diff = Span.(-)
let abs_diff t u = Span.abs (diff t u)

let to_span_since_epoch t = t
let of_span_since_epoch s = s

let to_int63_ns_since_epoch t = Span.to_int63_ns (to_span_since_epoch t)
let of_int63_ns_since_epoch i = of_span_since_epoch (Span.of_int63_ns i)

IFDEF ARCH_SIXTYFOUR THEN
let to_int_ns_since_epoch t = Core_int63.to_int_exn (to_int63_ns_since_epoch t)
let of_int_ns_since_epoch i = of_int63_ns_since_epoch (Core_int63.of_int i)
ELSE
let to_int_ns_since_epoch t = failwith "unsupported on 32bit machines"
let of_int_ns_since_epoch i = failwith "unsupported on 32bit machines"
ENDIF

let next_multiple ?(can_equal_after = false) ~base ~after ~interval () =
  if Span.(<=) interval Span.zero
  then failwiths "Time.next_multiple got nonpositive interval" interval
         <:sexp_of< Span.t >>;
  let base_to_after = diff after base in
  if Span.(<) base_to_after Span.zero
  then base (* [after < base], choose [k = 0]. *)
  else begin
    let next =
      add base
        (Span.scale interval
           (Float.round ~dir:`Down (Span.(//) base_to_after interval)))
    in
    if next > after || (can_equal_after && next = after)
    then next
    else add next interval
  end
;;

external nanosleep : float -> float = "core_kernel_time_ns_nanosleep" ;;

let nanosleep t = Span.of_sec (nanosleep (Span.to_sec t))

let pause_for t =
  let time_remaining =
    (* If too large a float is passed in (Span.max_value for instance) then nanosleep
       will return immediately, leading to an infinite and expensive select loop.  This
       is handled by pausing for no longer than 100 days. *)
    nanosleep (Span.min t (Span.scale Span.day 100.))
  in
  if Span.( > ) time_remaining Span.zero
  then `Remaining time_remaining
  else `Ok
;;

(** Pause and don't allow events to interrupt. *)
let rec pause span =
  match pause_for span with
  | `Remaining span -> pause span
  | `Ok -> ()
;;

(** Pause but allow events to interrupt. *)
let interruptible_pause = pause_for

let rec pause_forever () =
  pause Span.day;
  pause_forever ()
;;

module Alternate_sexp = struct
  module Sexp_repr = struct
    type t =
      { human_readable       : string
      ; int63_ns_since_epoch : Core_int63.t
      }
    with sexp

    let time_format = "%Y-%m-%dT%H:%M:%S%z"

    let of_time time =
      { human_readable =
          Platform_specific.strftime (Unix.localtime (Span.to_sec time)) time_format
      ; int63_ns_since_epoch = to_int63_ns_since_epoch time
      }

    let to_time t = of_int63_ns_since_epoch t.int63_ns_since_epoch
  end

  type nonrec t = t

  let sexp_of_t t = Sexp_repr.sexp_of_t (Sexp_repr.of_time t)
  let t_of_sexp s = Sexp_repr.to_time (Sexp_repr.t_of_sexp s)
end
