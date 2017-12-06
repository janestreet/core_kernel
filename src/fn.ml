(** Extends {!Base.Fn}. *)

include Deprecate_pipe_bang
include Base.Fn (** @open *)
