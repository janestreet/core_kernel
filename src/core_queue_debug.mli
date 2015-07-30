module Debug (Core_queue : module type of Core_queue) : sig

  (** The following [include] exposes the type equivalence [Debug(Queue).t = Queue.t]. *)
  include module type of struct include Core_queue end

  val check_invariant : bool ref
  val show_messages   : bool ref

end
