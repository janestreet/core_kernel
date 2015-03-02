open Timing_wheel_intf

module Make
         (Time         : Timing_wheel_time)
         (Timing_wheel : Timing_wheel with module Time := Time)
  : sig
    include module type of struct include Timing_wheel end

    val check_invariant : bool ref
    val show_messages   : bool ref
  end
