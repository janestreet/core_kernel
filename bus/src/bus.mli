(** A [Bus] is a publisher/subscriber system within the memory space of the program. A bus
    has a mutable set of subscribers, which can be modified using [subscribe_exn] and
    [unsubscribe].

    [create] returns a [Bus.Read_write.t], which you can use to [write] values to the bus.
    [write] calls the callbacks of all current subscribers before returning. In a
    [('callback, 'phantom) Bus.t], ['phantom] is a read-write phantom type that controls
    whether one can read values from or write values to the bus. The phantom type states
    the capabilities one could ever have access to, not the capabilities that are
    immediately available. *)

open! Core

type ('callback, -'phantom) t [@@deriving sexp_of]
type ('callback, -'phantom) bus = ('callback, 'phantom) t

module Read_write : sig
  type 'callback t = ('callback, read_write) bus [@@deriving sexp_of]

  include Invariant.S1 with type 'a t := 'a t
end

module Read_only : sig
  type 'callback t = ('callback, read) bus [@@deriving sexp_of]

  include Invariant.S1 with type 'a t := 'a t
end

module On_subscription_after_first_write : sig
  type t =
    | Allow
    | Allow_and_send_last_value_if_global
    | Raise
  [@@deriving compare, enumerate, equal, sexp_of]
end

val read_only : ('callback, _) t -> 'callback Read_only.t

(** In [create_exn ~on_subscription_after_first_write ~on_callback_raise ()], [[%here]] is
    stored in the resulting bus, and contained in [%sexp_of: t], which can help with
    debugging.

    If [on_subscription_after_first_write] is [Raise], then [subscribe_exn] will raise if
    it is called after [write] has been called the first time. If
    [on_subscription_after_first_write] is [Allow_and_send_last_value_if_global], then the
    bus will remember the last value written and will send it to new subscribers; values
    written with [write*_local] cannot be saved, and so the last value is not sent in this
    case.

    If a callback raises, [on_callback_raise] is called with an error containing the
    exception.

    If [on_callback_raise] raises, then the exception is raised to [write] and the bus is
    closed.

    ['callback] is the function type of subscribers to the bus. *)
val create_exn
  :  ?name:Info.t
  -> here:[%call_pos]
  -> on_subscription_after_first_write:On_subscription_after_first_write.t
  -> on_callback_raise:(Error.t -> unit)
  -> unit
  -> 'callback Read_write.t

val num_subscribers : (_, _) t -> int
val is_closed : (_, _) t -> bool

(** [close] disallows future [write]s -- once [close t] is called, all further calls to
    [write t] will raise. [close] is idempotent. If [close] is called from within a
    callback, the current message will still be sent to all subscribed callbacks that have
    not yet seen it before the close takes effect. *)
val close : 'callback Read_write.t -> unit

(** [write] ... [write5] call all callbacks currently subscribed to the bus, with no
    guarantee on the order in which they will be called. [write] is non-allocating, though
    the callbacks themselves may allocate. Calling [writeN t] raises if called from within
    a callback on [t] or when [is_closed t]. *)

val write : ('a -> unit) Read_write.t -> 'a -> unit

val%template write_local
  : ('a : ka).
  (local_ 'a -> unit) Read_write.t -> local_ 'a -> unit
[@@kind ka = base_or_null]

val write2 : ('a -> 'b -> unit) Read_write.t -> 'a -> 'b -> unit

val%template write2_local
  : ('a : ka) ('b : kb).
  (local_ 'a -> local_ 'b -> unit) Read_write.t -> local_ 'a -> local_ 'b -> unit
[@@kind ka = base_or_null, kb = base_or_null]

val write3 : ('a -> 'b -> 'c -> unit) Read_write.t -> 'a -> 'b -> 'c -> unit

val write3_local
  :  (local_ 'a -> local_ 'b -> local_ 'c -> unit) Read_write.t
  -> local_ 'a
  -> local_ 'b
  -> local_ 'c
  -> unit

val write4 : ('a -> 'b -> 'c -> 'd -> unit) Read_write.t -> 'a -> 'b -> 'c -> 'd -> unit

val write4_local
  :  (local_ 'a -> local_ 'b -> local_ 'c -> local_ 'd -> unit) Read_write.t
  -> local_ 'a
  -> local_ 'b
  -> local_ 'c
  -> local_ 'd
  -> unit

val write5
  :  ('a -> 'b -> 'c -> 'd -> 'e -> unit) Read_write.t
  -> 'a
  -> 'b
  -> 'c
  -> 'd
  -> 'e
  -> unit

val write5_local
  :  (local_ 'a -> local_ 'b -> local_ 'c -> local_ 'd -> local_ 'e -> unit) Read_write.t
  -> local_ 'a
  -> local_ 'b
  -> local_ 'c
  -> local_ 'd
  -> local_ 'e
  -> unit

val write6
  :  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> unit) Read_write.t
  -> 'a
  -> 'b
  -> 'c
  -> 'd
  -> 'e
  -> 'f
  -> unit

val write6_local
  :  (local_ 'a -> local_ 'b -> local_ 'c -> local_ 'd -> local_ 'e -> local_ 'f -> unit)
       Read_write.t
  -> local_ 'a
  -> local_ 'b
  -> local_ 'c
  -> local_ 'd
  -> local_ 'e
  -> local_ 'f
  -> unit

module Subscriber : sig
  type 'callback t [@@deriving sexp_of]
end

(** [subscribe_exn t ~f] adds the callback [f] to the set of [t]'s subscribers, and
    returns a [Subscriber.t] that can later be used to [unsubscribe]. [[%here]] is stored
    in the [Subscriber.t], and contained in [%sexp_of: Subscriber.t], which can help with
    debugging. If [subscribe_exn t] is called by a callback in [t], i.e., during
    [write t], the subscription takes effect for the next [write], but does not affect the
    current [write]. [subscribe_exn] takes amortized constant time.

    If [on_callback_raise] is supplied, then it will be called by [write] whenever [f]
    raises; only if that subsequently raises will [t]'s [on_callback_raise] be called. If
    [on_callback_raise] is not supplied, then [t]'s [on_callback_raise] will be called
    whenever [f] raises.

    If [on_callback_raise] is supplied and [extract_exn] is set to true, then the error
    passed to the [on_callback_raise] method will contain only the exception raised by [f]
    without any additional information about the bus subscription or backtrace.

    [on_close] is called if you are still subscribed when [Bus.close] is called. *)
val subscribe_exn
  :  ?extract_exn:bool (** default is [false] *)
  -> ?on_callback_raise:(Error.t -> unit)
  -> ?on_close:(unit -> unit)
  -> here:[%call_pos]
  -> ('callback, [> read ]) t
  -> f:'callback
  -> 'callback Subscriber.t

(** [subscribe_permanently_exn t ~f] is [ignore (subscribe_exn t ~callback:f)]. This
    captures the common usage in which one never wants to unsubscribe from a bus. *)
val subscribe_permanently_exn
  :  ?extract_exn:bool (** passed along to [subscribe_exn] *)
  -> here:[%call_pos]
  -> ('callback, [> read ]) t
  -> f:'callback
  -> unit

module Fold_arity : sig
  type (_, _, _) t =
    | Arity1 : ('a -> unit, 's -> 'a -> 's, 's) t
    | Arity2 : ('a -> 'b -> unit, 's -> 'a -> 'b -> 's, 's) t
    | Arity3 : ('a -> 'b -> 'c -> unit, 's -> 'a -> 'b -> 'c -> 's, 's) t
    | Arity4 : ('a -> 'b -> 'c -> 'd -> unit, 's -> 'a -> 'b -> 'c -> 'd -> 's, 's) t
    | Arity5 :
        ('a -> 'b -> 'c -> 'd -> 'e -> unit, 's -> 'a -> 'b -> 'c -> 'd -> 'e -> 's, 's) t
    | Arity6 :
        ( 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> unit
          , 's -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 's
          , 's )
          t
  [@@deriving sexp_of]
end

(** [subscribe_with_state_exn t arity ~init ~f] folds over the bus events, threading a
    state value to every call. This subscription can be stopped by unsubscribing the
    returned Subscriber.t. It is otherwise similar to [subscribe_exn]. *)
val subscribe_with_state_exn
  :  ?extract_exn:bool (** passed along to [subscribe_exn] *)
  -> here:[%call_pos]
  -> ('callback, [> read ]) t
  -> ('callback, 'f, 's) Fold_arity.t
  -> init:'s
  -> f:'f
  -> 'callback Subscriber.t

(** [subscribe_permanently_with_state_exn t arity ~init ~f] folds over the bus events,
    threading a state value to every call. It is otherwise similar to
    [subscribe_permanently_exn]. *)
val subscribe_permanently_with_state_exn
  :  ?extract_exn:bool (** passed along to [subscribe_exn] *)
  -> here:[%call_pos]
  -> ('callback, [> read ]) t
  -> ('callback, 'f, 's) Fold_arity.t
  -> init:'s
  -> f:'f
  -> unit

(** [unsubscribe t subscriber] removes the callback corresponding to [subscriber] from
    [t]. [unsubscribe] never raises and is idempotent. As with [subscribe_exn],
    [unsubscribe t] during [write t] takes effect after the current [write] finishes. Also
    like [subscribe_exn], [unsubscribe] takes amortized constant time. *)
val unsubscribe : ('callback, [> read ]) t -> 'callback Subscriber.t -> unit
