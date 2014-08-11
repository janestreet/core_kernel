(** A polymorphic hashtbl that uses [Pool] to avoid allocation.

    This uses the standard linked-chain hashtable algorithm, albeit with links performed
    through a pool and hence avoiding [caml_modify] (for table manipulation), even when
    hashing object keys/values.

    This implementation is worth exploring for your application if profiling demonstrates
    that garbage collection and the [caml_modify] write barrier are a significant part of
    your execution time. *)

include Core_hashtbl_intf.Hashtbl

(** [resize t size] ensures that [t] can hold at least [size] entries without resizing
    (again), provided that [t] has growth enabled.  This is useful for sizing global
    tables during application initialization, to avoid subsequent, expensive growth
    online.  See {!Jane_parachute.Immediate.String.resize}, for example. *)
val resize : (_, _) t -> int -> unit

(** Allow to connect higher level loggers to the point where these hashtbls grow. *)
val on_grow
  : (unix_time_before : float -> old_capacity : int -> int -> unit) -> unit
