open! Core_kernel.Std

module Debug (Queue : module type of Queue) : sig

  (** The following [include] exposes the type equivalence [Debug(Queue).t = Queue.t]. *)
  include module type of struct include Queue end

  val check_invariant : bool ref
  val show_messages   : bool ref

end
