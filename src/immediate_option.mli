(** A non-allocating alternative to the standard Option type. *)

include Immediate_option_intf.Immediate_option (** @inline *)
