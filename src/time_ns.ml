#import "config.mlh"
open Std_internal


let round_nearest = Float.int63_round_nearest_exn

let float x = Core_int63.to_float x

(* This signature constraint is semi-temporary and serves to make the implementation more
   type-safe (so the compiler can help us more).  It would go away if we broke the
   implementation into multiple files. *)
module Span : sig
  (* Note that the [sexp] below is implemented only for some debug text later in this
     module. It is not exposed in the mli. *)
  type t = Core_int63.t [@@deriving typerep, compare, bin_io, sexp]

  include Comparable.Infix     with type t := t
  include Comparable.Validate  with type t := t
  include Comparable.With_zero with type t := t
  include Equal.S              with type t := t

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

  val of_sec_with_microsecond_precision : float -> t

  val of_int_us  : int -> t
  val of_int_ms  : int -> t
  val of_int_sec : int -> t
  val to_int_us  : t -> int
  val to_int_ms  : t -> int
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
  val max : t -> t -> t

  val scale       : t -> float        -> t
  val scale_int   : t -> int          -> t
  val scale_int63 : t -> Core_int63.t -> t

  val div    : t -> t     -> Core_int63.t
  val ( / )  : t -> float -> t
  val ( // ) : t -> t     -> float

  val create
    :  ?sign : Sign.t
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
      { sign : Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    [@@deriving sexp]
  end

  val to_parts : t -> Parts.t
  val of_parts : Parts.t -> t

  include Robustly_comparable with type t := t

  val since_unix_epoch : unit -> t

  module Alternate_sexp : sig
    type nonrec t = t [@@deriving sexp]
  end
end = struct
  (* [Span] is basically a [Core_int63].  It even silently ignores overflow. *)
  module T = struct
    type t = Core_int63.t (** nanoseconds *)
    [@@deriving bin_io, typerep]

    let compare = Core_int63.compare
    let equal   = Core_int63.equal
    let zero    = Core_int63.zero

    include (Core_int63 : Comparable.Infix with type t := t)
  end

  include T

  module Parts = struct
    type t =
      { sign : Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    [@@deriving sexp]

    let compare = Poly.compare
  end

  let nanosecond  = Core_int63.of_int 1
  let microsecond = Core_int63.(of_int 1000 * nanosecond)
  let millisecond = Core_int63.(of_int 1000 * microsecond)
  let second      = Core_int63.(of_int 1000 * millisecond)
  let minute      = Core_int63.(of_int 60 * second)
  let hour        = Core_int63.(of_int 60 * minute)
  let day         = Core_int63.(of_int 24 * hour)

  (* Beyond [min_value..max_value], not every microsecond can be represented as a [float]
     number of seconds.  (In fact, it is around 135y, but we leave a small margin.)

     In the presence of silently ignored overflow, note that [t] is not actually bound to
     stay between these limits. *)
  let max_value = Core_int63.(of_int 135 * of_int 365 * day)
  let min_value = Core_int63.neg max_value

  let create
        ?sign:(sign_  = Sign.Pos) (* rebind so not shadowed by [open Core_int63] below *)
        ?day:(days    = 0)
        ?(hr          = 0)
        ?min:(minutes = 0)
        ?(sec         = 0)
        ?(ms          = 0)
        ?(us          = 0)
        ?(ns          = 0)
        () =
    let open Core_int63 in
    let t =
        of_int days    * day
      + of_int hr      * hour
      + of_int minutes * minute
      + of_int sec     * second
      + of_int ms      * millisecond
      + of_int us      * microsecond
      + of_int ns      * nanosecond
    in
    match sign_ with Neg -> neg t | Pos | Zero -> t
  ;;

  let to_parts t =
    let open Core_int63 in
    let mag = abs t in
    { Parts.
      sign = if t < zero then Neg else if t > zero then Pos else Zero
    ; hr = to_int_exn (mag / hour)
    ; min = to_int_exn ((rem mag hour) / minute)
    ; sec = to_int_exn ((rem mag minute) / second)
    ; ms = to_int_exn ((rem mag second) / millisecond)
    ; us = to_int_exn ((rem mag millisecond) / microsecond)
    ; ns = to_int_exn ((rem mag microsecond) / nanosecond)
    }
  ;;

  let of_parts { Parts. sign; hr; min; sec; ms; us; ns } =
    create ~sign ~hr ~min ~sec ~ms ~us ~ns ()
  ;;

  let of_ns       f = round_nearest f
  let of_int63_ns i = i
  let of_int_us   i = Core_int63.(of_int i * microsecond)
  let of_int_ms   i = Core_int63.(of_int i * millisecond)
  let of_int_sec  i = Core_int63.(of_int i * second)
  let of_us       f = round_nearest (f *. float microsecond)
  let of_ms       f = round_nearest (f *. float millisecond)
  let of_sec      f = round_nearest (f *. float second)
  let of_min      f = round_nearest (f *. float minute)
  let of_hr       f = round_nearest (f *. float hour)
  let of_day      f = round_nearest (f *. float day)

  let of_sec_with_microsecond_precision sec =
    let us = round_nearest (sec *. 1e6) in
    of_int63_ns Core_int63.(us * of_int 1000)
  ;;

  let to_ns       t = float t
  let to_int63_ns t =       t
  let to_us       t = float t /. float microsecond
  let to_ms       t = float t /. float millisecond
  let to_sec      t = float t /. float second
  let to_min      t = float t /. float minute
  let to_hr       t = float t /. float hour
  let to_day      t = float t /. float day
  let to_int_us   t = Core_int63.(to_int_exn (t / microsecond))
  let to_int_ms   t = Core_int63.(to_int_exn (t / millisecond))
  let to_int_sec  t = Core_int63.(to_int_exn (t / second))

#if JSC_ARCH_SIXTYFOUR
  let%test _ = Int.(>) (to_int_sec Core_int63.max_value) 0 (* and doesn't raise *)

  let of_int_ns i = of_int63_ns (Core_int63.of_int i)
  let to_int_ns t = Core_int63.to_int_exn (to_int63_ns t)
#else
  let of_int_ns _i = failwith "unsupported on 32bit machines"
  let to_int_ns _i = failwith "unsupported on 32bit machines"
#endif

  let (+)         t u = Core_int63.(+) t u
  let (-)         t u = Core_int63.(-) t u
  let abs             = Core_int63.abs
  let neg             = Core_int63.neg
  let scale       t f = round_nearest (float t *. f)
  let scale_int63 t i = Core_int63.( * ) t i
  let scale_int   t i = scale_int63 t (Core_int63.of_int i)
  let div             = Core_int63.( /% )
  let (/)         t f = round_nearest (float t /. f)
  let (//)            = Core_int63.(//)
  let max             = Core_int63.max

  let%test_module "overflow silently" =
    (module struct
      let doesn't_raise = Fn.non Exn.does_raise

      let%test "+ range up"   = doesn't_raise (fun () -> max_value +     nanosecond)
      let%test "+ range down" = doesn't_raise (fun () -> min_value + neg nanosecond)
      let%test "+ overflow"   = doesn't_raise (fun () -> max_value +     max_value)
      let%test "+ underflow"  = doesn't_raise (fun () -> min_value +     min_value)
      let%test "- range down" = doesn't_raise (fun () -> min_value -     nanosecond)
      let%test "- range up"   = doesn't_raise (fun () -> max_value - neg nanosecond)
      let%test "- underflow"  = doesn't_raise (fun () -> min_value -     max_value)
      let%test "- overflow"   = doesn't_raise (fun () -> max_value -     min_value)

      let%test_module "intermediate ( * )" =
        (module struct
          let wrap_days =
            let margin_ns = Core_int63.(-) min_value Core_int63.min_value in
            Core_int63.(max_value / day + one + margin_ns / day + one)
          ;;
          let%test_unit "wrap_days" =
            [%test_pred: Core_int63.t]
              (Core_int63.between ~low:min_value ~high:(Core_int63.neg nanosecond))
              Core_int63.(wrap_days * day)
          ;;
          let wrap_days_int = Core_int63.to_int_exn wrap_days
          let%test "scale_int63" = doesn't_raise (fun () -> scale_int63 day wrap_days)
          let%test "scale_int"   = doesn't_raise (fun () -> scale_int   day wrap_days_int)
          let%test "create"      = doesn't_raise (fun () -> create ()  ~day:wrap_days_int)
        end)
      ;;
    end)
  ;;

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

  include Comparable.Validate_with_zero(struct
    include T
    let sexp_of_t = Alternate_sexp.sexp_of_t
    let t_of_sexp = Alternate_sexp.t_of_sexp
  end)

  let%test_module _ = (module struct
    let ( * ) = Core_int63.( * )
    let of_int = Core_int63.of_int

    let round_trip t = [%test_result: t] (of_parts (to_parts t)) ~expect:t
    let eq t expect =
      [%test_result: t] t ~expect;
      [%test_result: Parts.t] (to_parts t) ~expect:(to_parts expect);
      round_trip t

    let%test_unit _ = eq (create ~us:2            ()) (of_int 2    * microsecond)
    let%test_unit _ = eq (create ~min:3           ()) (of_int 3    * minute)
    let%test_unit _ = eq (create ~ms:4            ()) (of_int 4    * millisecond)
    let%test_unit _ = eq (create ~sec:5           ()) (of_int 5    * second)
    let%test_unit _ = eq (create ~hr:6            ()) (of_int 6    * hour)
    let%test_unit _ = eq (create ~day:7           ()) (of_int 7    * day)
    let%test_unit _ = eq (create ~us:8 ~sign:Neg  ()) (of_int (-8) * microsecond)
    let%test_unit _ = eq (create ~ms:9 ~sign:Zero ()) (of_int 9    * millisecond)
    let%test_unit _ =
      eq (create ~us:3 ~ns:242 () |> to_sec |> of_sec_with_microsecond_precision)
        (of_int 3 * microsecond)
    let%test_unit _ =
      for _ = 1 to 1_000_000 do
        let t =
          (Core_int63.of_int64_exn (Random.int64 (Core_int63.to_int64 max_value)))
          + if Random.bool () then zero else min_value
        in
        round_trip t
      done

    let round_trip parts =
      [%test_result: Parts.t] (to_parts (of_parts parts)) ~expect:parts
    let eq parts expect =
      [%test_result: Parts.t] parts ~expect;
      [%test_result: t] (of_parts parts) ~expect:(of_parts expect);
      round_trip parts

    let%test_unit _ =
      eq (to_parts (create ~sign:Neg ~hr:2 ~min:3 ~sec:4 ~ms:5 ~us:6 ~ns:7 ()))
        { Parts. sign = Neg; hr = 2; min = 3; sec = 4; ms = 5; us = 6; ns = 7 }
    let%test_unit _ = round_trip (to_parts (create ~hr:25 ()))
    let%test_unit _ =
      let hr =
        match Word_size.word_size with
        | W32 -> Core_int.max_value
        | W64 -> Core_int64.to_int_exn 2217989799822798757L
      in
      round_trip (to_parts (create ~hr ()))
  end)

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

#if JSC_ARCH_SIXTYFOUR
  external since_unix_epoch_or_zero : unit -> t
    = "core_kernel_time_ns_gettime_or_zero" "noalloc"
#else
  external since_unix_epoch_or_zero : unit -> t
    = "core_kernel_time_ns_gettime_or_zero"
#endif

#if JSC_POSIX_TIMERS
  let gettime_failed () = failwith "clock_gettime(CLOCK_REALTIME) failed"
#else
  let gettime_failed () = failwith "gettimeofday failed"
#endif

  let since_unix_epoch () =
    let t = since_unix_epoch_or_zero () in
    if t <> zero then t else gettime_failed ()
  ;;
end

type t = Span.t (** since the Unix epoch (1970-01-01 00:00:00 UTC) *)
[@@deriving bin_io, compare, typerep]

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
let max = Span.max

let to_span_since_epoch t = t
let of_span_since_epoch s = s

let to_int63_ns_since_epoch t = Span.to_int63_ns (to_span_since_epoch t)
let of_int63_ns_since_epoch i = of_span_since_epoch (Span.of_int63_ns i)

#if JSC_ARCH_SIXTYFOUR
let to_int_ns_since_epoch t = Core_int63.to_int_exn (to_int63_ns_since_epoch t)
let of_int_ns_since_epoch i = of_int63_ns_since_epoch (Core_int63.of_int i)
#else
let to_int_ns_since_epoch _t = failwith "unsupported on 32bit machines"
let of_int_ns_since_epoch _i = failwith "unsupported on 32bit machines"
#endif

let next_multiple ?(can_equal_after = false) ~base ~after ~interval () =
  if Span.(<=) interval Span.zero
  then failwiths "Time.next_multiple got nonpositive interval" interval
         [%sexp_of: Span.t];
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

module Alternate_sexp = struct
  module Sexp_repr = struct
    type t =
      { human_readable       : string
      ; int63_ns_since_epoch : Core_int63.t
      }
    [@@deriving sexp]

    let time_format = "%Y-%m-%dT%H:%M:%S%z"

    (* We have pulled this up here so that we have a way for formatting times in their sexp
       representation. *)
    external format : float -> string -> string = "core_kernel_time_ns_format"

    let of_time time =
      { human_readable = format (Span.to_sec time) time_format
      ; int63_ns_since_epoch = to_int63_ns_since_epoch time
      }

    let to_time t = of_int63_ns_since_epoch t.int63_ns_since_epoch
  end

  type nonrec t = t

  let sexp_of_t t = Sexp_repr.sexp_of_t (Sexp_repr.of_time t)
  let t_of_sexp s = Sexp_repr.to_time (Sexp_repr.t_of_sexp s)
end
