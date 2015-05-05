open Timing_wheel_intf

include Timing_wheel
  (* We would like to use [with module Time = Time_ns], but can't because that requires
     certain values to be present in [Time_ns] that aren't there. *)
  with type Time.t = Time_ns.t
  with type Time.Span.t = Time_ns.Span.t
