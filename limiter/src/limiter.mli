(** Implements a token-bucket-based throttling rate limiter. This module is useful for
    limiting network clients to a sensible query rate, or in any case where you have jobs
    that consume a scarce but replenishable resource.

    In a standard token bucket there is an infinite incoming supply of tokens that fill a
    single bucket.

    This version implements a closed system where tokens move through three possible
    states:

    - in hopper
    - in bucket
    - in flight

    Tokens "drop" from the hopper into the bucket at a set rate, and can be taken from the
    bucket by clients and put into flight. Once the client is finished with whatever
    tokens are required for its task, it is responsible for moving them from "in flight"
    back into the hopper.

    Most use cases are covered by the [Token_bucket], [Throttle], and
    [Throttled_rate_limiter] modules, but the [Expert] module provides full access to the
    module internals.

    This interface is the simple, non-concurrent interface, and requires machinery on top
    to implement a specific strategy. See [Limiter_async] for an async-friendly
    implementation on top of this module.

    Most functions in this interface take an explicit time as an argument. [now] is
    expected to be monotonically increasing. [now]'s that are set in the past are
    effectively moved up to the current time of the bucket. *)

open! Core
open! Import

type t [@@deriving sexp_of]
type limiter = t [@@deriving sexp_of]

module Infinite_or_finite : sig
  type 'a t =
    | Infinite
    | Finite of 'a
  [@@deriving sexp, bin_io, compare ~localize]
end

module Try_take_result : sig
  type t =
    | Taken
    | Unable
    | Asked_for_more_than_bucket_limit
end

module Try_return_to_bucket_result : sig
  type t =
    | Returned_to_bucket
    | Unable
end

module Tokens_may_be_available_result : sig
  type t =
    | At of Time_ns.t
    | Never_because_greater_than_bucket_limit
    | When_return_to_hopper_is_called
end

module Try_reconfigure_result : sig
  type t =
    | Reconfigured
    | Unable
  [@@deriving sexp_of]
end

(** Implements a basic token-bucket-based rate limiter. Users of the throttle must
    successfully call [try_take] before doing work. *)
module Token_bucket : sig
  type t = private limiter [@@deriving sexp_of]

  (** @param initial_bucket_level defaults to zero. *)
  val create_exn
    :  now:Time_ns.t
    -> burst_size:int
    -> sustained_rate_per_sec:float
    -> ?initial_bucket_level:int
    -> unit
    -> t

  val try_take : t -> now:Time_ns.t -> int -> Try_take_result.t

  module Starts_full : sig
    type nonrec t = private t [@@deriving sexp_of]

    (** A [Token_bucket.Starts_full.t] is a [Token_bucket.t] that is statically guaranteed
        to have been called with [initial_bucket_level] equal to [burst_size]. The
        advantage of such a guarantee is that there's a clear semantics for increasing the
        bucket limit (implemented in [try_increase_bucket_limit]).

        This is not to say that other subtypes of [Limiter.t] don't have reasonable
        semantics for increasing their limits in some way, but [Limiter.t] is general
        enough that they should probably be considered on a case-by-case basis. *)
    val create_exn : now:Time_ns.t -> burst_size:int -> sustained_rate_per_sec:float -> t

    (** Increases the [bucket_limit] and the current [bucket_level] by the difference
        between the current and new bucket limits. Decreasing the bucket_limit may cause
        the [bucket_level] to become negative, breaking an invariant. If the new limit
        would cause [bucket_level] to become negative, [Unable] is returned. *)
    val try_reconfigure
      :  t
      -> burst_size:int
      -> sustained_rate_per_sec:float
      -> allow_limit_decrease:bool
      -> Try_reconfigure_result.t
  end
end

(** Implements a basic throttle. Users of the throttle must successfully call [start_job]
    before beginning work and must call [finish_job] once, and only once, when a job is
    completed. *)
module Throttle : sig
  type t = private limiter [@@deriving sexp_of]

  val create_exn : now:Time_ns.t -> max_concurrent_jobs:int -> t
  val try_start_job : t -> now:Time_ns.t -> [ `Start | `Max_concurrent_jobs_running ]
  val finish_job : t -> now:Time_ns.t -> unit
end

(** A [Throttled_rate_limiter] combines a [Token_bucket] and a [Throttle]. Unlike a
    [Token_bucket], jobs cannot consume variable numbers of tokens, but the number of
    outstanding jobs is also limited to [max_concurrent_jobs]. Like a [Throttle],
    [finish_job] must be called once, and only once, when a job is completed. *)
module Throttled_rate_limiter : sig
  type t = private limiter [@@deriving sexp_of]

  val create_exn
    :  now:Time_ns.t
    -> burst_size:int
    -> sustained_rate_per_sec:float
    -> max_concurrent_jobs:int
         (** Limits concurrency per time quantum. Any job started during a time quantum
             ([try_start_job ~now]) or earlier and not stopped in an earlier quantum
             counts toward the concurrency limit.

             In particular, [finish_job ~now] never prevents a job from counting toward
             [~now]'s concurrency limit. *)
    -> t

  val try_start_job
    :  t
    -> now:Time_ns.t
    -> [ `Start | `Max_concurrent_jobs_running | `Unable_until_at_least of Time_ns.t ]

  (** Return a token to the {e hopper} (not the bucket). Thus, [max_concurrent_jobs]
      limits not only the number of open [try_start_job]-[finish_job] pairs across time,
      but also applies to the number of jobs run during the same (1ns) quantum time
      [now] - whether they finished [now] or not, and regardless of what order
      [finish_job] is called for the same time [now]. *)
  val finish_job : t -> now:Time_ns.t -> unit
end

(** {2 Common read-only operations} *)

val bucket_limit : t -> int

(** Tokens available to immediately take. *)
val in_bucket : t -> now:Time_ns.t -> int

(** Tokens waiting to drop at the [hopper_to_bucket_rate_per_sec]. *)
val in_hopper : t -> now:Time_ns.t -> int Infinite_or_finite.t

(** Tokens that have been taken, but not yet returned. *)
val in_flight : t -> now:Time_ns.t -> int

(** Total number of tokens in the limiter [in_hopper + in_bucket]. *)
val in_limiter : t -> now:Time_ns.t -> int Infinite_or_finite.t

(** Total number of tokens in the entire system [in_hopper + in_bucket + in_flight]. *)
val in_system : t -> now:Time_ns.t -> int Infinite_or_finite.t

(** Note that this isn't guaranteed to be equal to the [rate_per_sec] that was passed in
    to the constructor, due to floating point error. *)
val hopper_to_bucket_rate_per_sec : t -> float Infinite_or_finite.t

(** Expert operations. *)
module Expert : sig
  (** @param now
        is the reference time that other time-accepting functions will use when they
        adjust [now]. It is almost always correct to set this to [Time_ns.now].

      @param hopper_to_bucket_rate_per_sec
        bounds the maximum rate at which tokens fall from the hopper into the bucket where
        they can be taken.

      @param bucket_limit
        bounds the number of tokens that the lower bucket can hold. This corresponds to
        the maximum burst in a standard token bucket setup.

      @param in_flight_limit
        bounds the number of tokens that can be in flight. This corresponds to a running
        job limit/throttle.

      @param initial_hopper_level
        sets the number of tokens placed into the hopper when the [Limiter] is created.

      @param initial_bucket_level
        sets the number of tokens placed into the bucket when the [Limiter] is created. If
        this amount exceeds the bucket size it will be silently limited to [bucket_limit].

        These tunables can be combined in several ways:

        - to produce a simple rate limiter, where the hopper is given an infinite number
          of tokens and clients simply take tokens as they are delivered to the bucket.
        - to produce a rate limiter that respects jobs that are more than instantaneous.
          In this case [initial_hopper_level + initial_bucket_level] should be bounded and
          clients hold tokens for the duration of their work.
        - to produce a throttle that doesn't limit the rate of jobs at all, but always
          keeps a max of n jobs running. In this case [hopper_to_bucket_rate_per_sec]
          should be infinite but [in_flight_limit] should be bounded to the upper job
          rate.

        In every case above, throttling and rate limiting combine nicely when the unit of
        work for both is the same (e.g., one token per message). If the unit of work is
        different (e.g., rate limit based on a number of tokens equal to message size, but
        throttle based on simple message count) then a single [t] probably cannot be used
        to get the correct behavior, and two instances should be used with tokens taken
        from both. *)
  val create_exn
    :  now:Time_ns.t
    -> hopper_to_bucket_rate_per_sec:float Infinite_or_finite.t
    -> bucket_limit:int
    -> in_flight_limit:int Infinite_or_finite.t
    -> initial_bucket_level:int
    -> initial_hopper_level:int Infinite_or_finite.t
    -> t

  (** Returns the earliest time when the requested number of tokens could possibly be
      delivered. There is no guarantee that the requested number of tokens will actually
      be available at this time. You must call [try_take] to actually attempt to take the
      tokens. *)
  val tokens_may_be_available_when
    :  t
    -> now:Time_ns.t
    -> int
    -> Tokens_may_be_available_result.t

  (** Attempts to take the given number of tokens from the bucket. [try_take t ~now n]
      succeeds iff [in_bucket t ~now >= n]. *)
  val try_take : t -> now:Time_ns.t -> int -> Try_take_result.t

  (** Returns the given number of tokens to the hopper. These tokens will fill the tokens
      available to [try_take] at the [fill_rate]. Note that if [return] is called on more
      tokens than have actually been removed, it can cause the number of concurrent jobs
      to exceed [max_concurrent_jobs]. *)
  val return_to_hopper : t -> now:Time_ns.t -> int -> unit

  (** Returns the given number of tokens directly to the bucket. If the amount is
      negative, is more than is currently in flight, or if moving the amount would cause
      the bucket to surpass its [bucket_limit], [Unable] is returned. *)
  val try_return_to_bucket : t -> now:Time_ns.t -> int -> Try_return_to_bucket_result.t
end

include Invariant.S with type t := t
