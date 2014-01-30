module Debug (Flat_queue : module type of Flat_queue) : sig

  include module type of Flat_queue

  val check_invariant : bool ref
  val show_messages   : bool ref

end
