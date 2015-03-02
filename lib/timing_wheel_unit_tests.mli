(** [Timing_wheel_unit_tests.Make] implements timing-wheel unit tests, and is used by both
    [Timing_wheel_float] and [Timing_wheel_ns]. *)

open Timing_wheel_intf

module Make (Timing_wheel : Timing_wheel)
  : sig
    open Timing_wheel

    (** [create_unit] is exposed for use in additional unit tests.  *)
    val create_unit
      :  ?level_bits      : Level_bits.t
      -> ?start           : Time.t
      -> ?alarm_precision : Time.Span.t
      -> unit
      -> unit t
  end
