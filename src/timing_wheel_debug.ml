open Std_internal
open Timing_wheel_intf

module Make
    (Time         : Timing_wheel_time)
    (Timing_wheel : Timing_wheel with module Time := Time) = struct

  module Debug = Debug.Make ()

  include Debug

  open Timing_wheel

  type nonrec 'a t = 'a t [@@deriving sexp_of]

  type nonrec 'a t_now = 'a t_now [@@deriving sexp_of]

  type 'a timing_wheel = 'a t

  module Interval_num = Interval_num

  module Level_bits = struct
    open Level_bits

    type nonrec t = t [@@deriving sexp]

    let invariant = invariant

    let debug x = Debug.debug invariant ~module_name:"Timing_wheel.Level_bits" x

    let max_num_bits = max_num_bits

    let create_exn ints =
      debug "create_exn" [] ints [%sexp_of: int list] [%sexp_of: t]
        (fun () -> create_exn ints)
    ;;

    let default = default

    let num_bits = num_bits
  end

  module Config = struct
    open Config

    type nonrec t = t [@@deriving sexp]

    let alarm_precision = alarm_precision
    let create          = create
    let durations       = durations
    let invariant       = invariant
    let level_bits      = level_bits
  end

  module Alarm = struct
    open Alarm

    type nonrec 'a t = 'a t [@@deriving sexp_of]

    let null = null

    let at           = at
    let interval_num = interval_num
    let value        = value
  end

  let invariant = invariant

  let debug x = Debug.debug (invariant ignore) ~module_name:"Timing_wheel" x

  let create ~config ~start =
    debug "create" [] (config, start) [%sexp_of: Config.t * Time.t]
      [%sexp_of: _ t]
      (fun () ->
         let t = create ~config ~start in
         invariant ignore t;
         t)
  ;;

  let alarm_precision = alarm_precision
  let now = now
  let start = start

  let is_empty = is_empty
  let length = length

  let iter t ~f =
    debug "iter" [t] () [%sexp_of: unit] [%sexp_of: unit]
      (fun () -> iter t ~f)
  ;;

  let interval_num t time =
    debug "interval_num" [t] time [%sexp_of: Time.t] [%sexp_of: Interval_num.t]
      (fun () -> interval_num t time)
  ;;

  let now_interval_num t =
    debug "now_interval_num" [t] () [%sexp_of: unit] [%sexp_of: Interval_num.t]
      (fun () -> now_interval_num t)
  ;;

  let interval_start t time =
    debug "interval_start" [t] time [%sexp_of: Time.t] [%sexp_of: Time.t]
      (fun () -> interval_start t time)
  ;;

  let interval_num_start t interval_num =
    debug "interval_num_start" [t] interval_num [%sexp_of: Interval_num.t] [%sexp_of: Time.t]
      (fun () -> interval_num_start t interval_num)
  ;;

  let advance_clock t ~to_ ~handle_fired =
    debug "advance_clock" [t] to_ [%sexp_of: Time.t] [%sexp_of: unit]
      (fun () -> advance_clock t ~to_ ~handle_fired)
  ;;

  let fire_past_alarms t ~handle_fired =
    debug "fire_past_alarms" [t] () [%sexp_of: unit] [%sexp_of: unit]
      (fun () -> fire_past_alarms t ~handle_fired)
  ;;

  let alarm_upper_bound = alarm_upper_bound

  let add t ~at a =
    debug "add" [t] at [%sexp_of: Time.t] [%sexp_of: _ Alarm.t]
      (fun () -> add t ~at a)
  ;;

  let add_at_interval_num t ~at a =
    debug "add_at_interval_num" [t] at [%sexp_of: Interval_num.t] [%sexp_of: _ Alarm.t]
      (fun () -> add_at_interval_num t ~at a)
  ;;

  let remove t alarm =
    debug "remove" [t] alarm [%sexp_of: _ Alarm.t] [%sexp_of: unit]
      (fun () -> remove t alarm)
  ;;

  let reschedule t alarm ~at =
    debug "reschedule" [t] (alarm, at) [%sexp_of: _ Alarm.t * Time.t] [%sexp_of: unit]
      (fun () -> reschedule t alarm ~at)
  ;;

  let reschedule_at_interval_num t alarm ~at =
    debug "reschedule_at_interval_num" [t] (alarm, at)
      [%sexp_of: _ Alarm.t * Interval_num.t]
      [%sexp_of: unit]
      (fun () -> reschedule_at_interval_num t alarm ~at)
  ;;

  let clear t =
    debug "clear" [t] () [%sexp_of: unit] [%sexp_of: unit]
      (fun () -> clear t)
  ;;

  let mem t alarm =
    debug "mem" [t] alarm [%sexp_of: _ Alarm.t] [%sexp_of: bool]
      (fun () -> mem t alarm)
  ;;

  let next_alarm_fires_at = next_alarm_fires_at

  let next_alarm_fires_at_exn = next_alarm_fires_at_exn

  module Priority_queue = struct
    open Priority_queue

    type nonrec 'a t = 'a t [@@deriving sexp_of]

    type 'a priority_queue = 'a t

    module Key = Key

    module Elt = struct
      open Elt

      type nonrec 'a t = 'a t [@@deriving sexp_of]

      let invariant = invariant
      let key = key
      let value = value
    end

    let invariant = invariant

    let debug x = Debug.debug (invariant ignore) ~module_name:"Priority_queue" x

    let create ?level_bits () =
      debug "create" [] level_bits [%sexp_of: Level_bits.t option] [%sexp_of: _ t]
        (fun () -> create ?level_bits ())
    ;;

    let length = length

    let is_empty = is_empty

    let min_allowed_key = min_allowed_key

    let max_allowed_key = max_allowed_key

    let min_elt t =
      debug "min_elt" [t] () [%sexp_of: unit] [%sexp_of: _ Elt.t option]
        (fun () -> min_elt t)
    ;;

    let min_key t =
      debug "min_key" [t] () [%sexp_of: unit] [%sexp_of: Key.t option]
        (fun () -> min_key t)
    ;;

    let add t ~key a =
      debug "add" [t] key [%sexp_of: Key.t] [%sexp_of: _ Elt.t]
        (fun () -> add t ~key a)
    ;;

    let remove t elt =
      debug "remove" [t] elt [%sexp_of: _ Elt.t] [%sexp_of: unit]
        (fun () -> remove t elt)
    ;;

    let change_key t elt ~key =
      debug "change_key" [t] (elt, key) [%sexp_of: _ Elt.t * Key.t] [%sexp_of: unit]
        (fun () -> change_key t elt ~key)
    ;;

    let clear t =
      debug "clear" [t] () [%sexp_of: unit] [%sexp_of: unit]
        (fun () -> clear t)
    ;;

    let mem t elt =
      debug "mem" [t] elt [%sexp_of: _ Elt.t] [%sexp_of: bool]
        (fun () -> mem t elt)
    ;;

    let increase_min_allowed_key t ~key ~handle_removed =
      debug "increase_min_allowed_key" [t] key [%sexp_of: Key.t] [%sexp_of: unit]
        (fun () -> increase_min_allowed_key t ~key ~handle_removed)
    ;;

    let iter = iter
  end
end
