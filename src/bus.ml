open Std_internal

module State = struct
  type t =
    | Closed
    | Write_in_progress
    | Ok_to_write
  [@@deriving sexp_of]

  let is_closed = function
    | Closed            -> true
    | Write_in_progress -> false
    | Ok_to_write       -> false
  ;;
end

module Callback_arity = struct
  type _ t =
    | Arity1 : ('a ->                   unit) t
    | Arity2 : ('a -> 'b ->             unit) t
    | Arity3 : ('a -> 'b -> 'c ->       unit) t
    | Arity4 : ('a -> 'b -> 'c -> 'd -> unit) t
  [@@deriving sexp_of]

  let dummy_callback (type a) (arity : a t) : a =
    match arity with
    | Arity1 -> (fun _       -> assert false)
    | Arity2 -> (fun _ _     -> assert false)
    | Arity3 -> (fun _ _ _   -> assert false)
    | Arity4 -> (fun _ _ _ _ -> assert false)
  ;;
end

module Subscriber_id = Unique_id.Int63 ()

module Subscriber = struct
  type 'callback t =
    { id                : Subscriber_id.t
    ; callback          : 'callback
    ; on_callback_raise : (Error.t -> unit) option
    ; subscribed_from   : Source_code_position.t
    }
  [@@deriving fields, sexp_of]

  let sexp_of_t _ t = Sexp.List [ Atom "Bus.Subscriber.t"; t |> [%sexp_of: _ t] ]

  let invariant invariant_a t =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~id:ignore
        ~callback:(check invariant_a)
        ~on_callback_raise:ignore
        ~subscribed_from:ignore)
  ;;

  let create subscribed_from ~callback ~on_callback_raise =
    { id                    = Subscriber_id.create ()
    ; callback
    ; on_callback_raise
    ; subscribed_from
    }
  ;;
end

type ('callback, 'phantom) t =
  (* Fields in this record are ordered to aid readability of [%sexp_of: t]. *)
  { name                                 : Info.t option
  ; callback_arity                       : 'callback Callback_arity.t
  ; created_from                         : Source_code_position.t
  ; allow_subscription_after_first_write : bool
  ; mutable state                        : State.t
  ; mutable write_ever_called            : bool
  ; mutable subscribers                  : 'callback Subscriber.t Subscriber_id.Map.t
  (* [t.write] is a closure closed over [t], that implements [Bus.write].  We recompute
     [write] everytime the set of subscribers changes.  This lets us allocate the closure
     during rare operations ([subscribe_exn]/[unsubscribe]), and reuse the closure without
     allocating during the common operation, [Bus.write]. *)
  ; mutable write                        : 'callback
  ; on_callback_raise                    : Error.t -> unit }
[@@deriving fields, sexp_of]

type ('callback, 'phantom) bus = ('callback, 'phantom) t [@@deriving sexp_of]

let read_only t = (t :> (_, read) t)

let invariant invariant_a _ t =
  Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~name:ignore
      ~callback_arity:ignore
      ~created_from:ignore
      ~allow_subscription_after_first_write:ignore
      ~state:ignore
      ~write_ever_called:ignore
      ~subscribers:(check (fun subscribers ->
        Map.iteri subscribers ~f:(fun ~key:_ ~data:callback ->
          Subscriber.invariant invariant_a callback)))
      ~write:ignore
      ~on_callback_raise:ignore)
;;

let is_closed t = State.is_closed t.state

let num_subscribers t = Map.length t.subscribers

module Read_write = struct
  type 'callback t = ('callback, read_write) bus [@@deriving sexp_of]

  let invariant invariant_a t = invariant invariant_a ignore t
end

module Read_only = struct
  type 'callback t = ('callback, read) bus [@@deriving sexp_of]

  let invariant invariant_a t = invariant invariant_a ignore t
end

let start_write t =
  t.write_ever_called <- true;
  match t.state with
  | Closed ->
    failwiths "Bus.Publisher.write called on closed bus" t [%sexp_of: (_, _) t];
  | Write_in_progress ->
    failwiths "Bus.Publisher.write called from callback on the same bus" t
      [%sexp_of: (_, _) t];
  | Ok_to_write ->
    t.state <- Write_in_progress
;;

let finish_write t =
  match t.state with
  | Closed -> ()
  | Ok_to_write -> assert false
  | Write_in_progress -> t.state <- Ok_to_write
;;

let close t =
  match t.state with
  | Closed -> ()
  | Ok_to_write | Write_in_progress ->
    t.state       <- Closed;
    t.subscribers <- Subscriber_id.Map.empty;
    t.write       <- Callback_arity.dummy_callback t.callback_arity;
;;

let update_write (type callback) (t : (callback, _) t) =
  (* Computing [callbacks] takes time proportional to the number of callbacks, which we
     have decided is OK because we expect subscription/unsubscription to be rare, and the
     number of callbacks to be few.  We could do constant-time update of the set of
     subscribers using a using a custom bag-like thing with a pair of arrays, one of the
     callbacks and one of the subscribers.  We've decided not to introduce that complexity
     until the performance benefit warrants it. *)
  let subscribers = t.subscribers |> Map.data |> Array.of_list in
  let callbacks   =   subscribers |> Array.map ~f:Subscriber.callback in
  let call_on_callback_raise error =
    try
      t.on_callback_raise error
    with exn ->
      close t;
      raise exn
  in
  let callback_raised i exn =
    let backtrace = Backtrace.Exn.most_recent () in
    (* [i] was incremented before the callback was called, so we have to subtract one
       here.  We do this here, rather than at the call site, because there are multiple
       call sites due to the optimazations needed to keep this zero-alloc. *)
    let subscriber = subscribers.( i - 1 ) in
    let error =
      [%message "Bus subscriber raised"
               (exn : exn) (backtrace : string) (subscriber : _ Subscriber.t)]
      |> [%of_sexp: Error.t]
    in
    match subscriber.on_callback_raise with
    | None -> call_on_callback_raise error
    | Some f ->
      try
        f error
      with exn ->
        let backtrace = Backtrace.Exn.most_recent () in
        call_on_callback_raise
          (let original_error = error in
           [%message "Bus subscriber's [on_callback_raise] raised"
                    (exn : exn) (backtrace : string) (original_error : Error.t)]
           |> [%of_sexp: Error.t])
  in
  let len = Array.length callbacks in
  (* [write] is closed over [callbacks] and iterates over that.  This makes it clear that
     the set of callbacks [write] calls isn't affected by calls to [update_write] during
     the [write].

     The code uses a double while-loop structure so that each callback can raise without
     preventing further callbacks from being called.  In the normal case this lets us only
     do the work to setup a single try/with for the full set of callbacks. *)
  let write : callback =
    match t.callback_arity with
    | Callback_arity.Arity1 -> (fun a1 ->
      start_write t;
      let i = ref 0 in
      while !i < len do
        try
          while !i < len do
            let callback = callbacks.( !i ) in
            incr i;
            callback a1;
          done
        with exn -> callback_raised !i exn
      done;
      finish_write t)
    | Callback_arity.Arity2 -> (fun a1 a2 ->
      start_write t;
      let i = ref 0 in
      while !i < len do
        try
          while !i < len do
            let callback = callbacks.( !i ) in
            incr i;
            callback a1 a2;
          done
        with exn -> callback_raised !i exn
      done;
      finish_write t)
    | Callback_arity.Arity3 -> (fun a1 a2 a3 ->
      start_write t;
      let i = ref 0 in
      while !i < len do
        try
          while !i < len do
            let callback = callbacks.( !i ) in
            incr i;
            callback a1 a2 a3;
          done
        with exn -> callback_raised !i exn
      done;
      finish_write t)
    | Callback_arity.Arity4 -> (fun a1 a2 a3 a4 ->
      start_write t;
      let i = ref 0 in
      while !i < len do
        try
          while !i < len do
            let callback = callbacks.( !i ) in
            incr i;
            callback a1 a2 a3 a4;
          done
        with exn -> callback_raised !i exn
      done;
      finish_write t)
  in
  t.write <- write;
;;

let create
      ?name
      created_from
      callback_arity
      ~allow_subscription_after_first_write
      ~on_callback_raise
  =
  let t =
    { name
    ; callback_arity
    ; created_from
    ; on_callback_raise
    ; allow_subscription_after_first_write
    ; subscribers                          = Subscriber_id.Map.empty
    ; write                                = Callback_arity.dummy_callback callback_arity
    ; state                                = Ok_to_write
    ; write_ever_called                    = false }
  in
  update_write t;
  t
;;

let can_subscribe t = t.allow_subscription_after_first_write || not t.write_ever_called

let subscribe_exn ?on_callback_raise t subscribed_from ~f =
  if not (can_subscribe t)
  then failwiths "Bus.subscribe_exn called after first write"
         [%sexp ~~(subscribed_from : Source_code_position.t), { bus = (t : (_, _) t) }]
         [%sexp_of: Sexp.t];
  let subscriber = Subscriber.create subscribed_from ~callback:f ~on_callback_raise in
  t.subscribers <- Map.add t.subscribers ~key:subscriber.id ~data:subscriber;
  update_write t;
  subscriber
;;

let iter_exn t subscribed_from ~f =
  if not (can_subscribe t)
  then failwiths "Bus.iter_exn called after first write" t [%sexp_of: (_, _) t];
  ignore (subscribe_exn t subscribed_from ~f : _ Subscriber.t);
;;

module Fold_arity = struct
  type (_, _, _) t =
    | Arity1 : ('a ->                   unit, 's -> 'a ->                   's, 's) t
    | Arity2 : ('a -> 'b ->             unit, 's -> 'a -> 'b ->             's, 's) t
    | Arity3 : ('a -> 'b -> 'c ->       unit, 's -> 'a -> 'b -> 'c ->       's, 's) t
    | Arity4 : ('a -> 'b -> 'c -> 'd -> unit, 's -> 'a -> 'b -> 'c -> 'd -> 's, 's) t
  [@@deriving sexp_of]
end

let fold_exn
      (type c) (type f) (type s)
      (t : (c, _) t)
      subscribed_from
      (fold_arity : (c, f, s) Fold_arity.t)
      ~(init : s)
      ~(f : f)
  =
  let state = ref init in
  if not (can_subscribe t)
  then failwiths "Bus.fold_exn called after first write" t [%sexp_of: (_, _) t];
  let module A = Fold_arity in
  iter_exn t subscribed_from ~f:(match fold_arity with
    | A.Arity1 -> fun a1          -> state := f !state a1
    | A.Arity2 -> fun a1 a2       -> state := f !state a1 a2
    | A.Arity3 -> fun a1 a2 a3    -> state := f !state a1 a2 a3
    | A.Arity4 -> fun a1 a2 a3 a4 -> state := f !state a1 a2 a3 a4)
;;

let unsubscribe t (subscription : _ Subscriber.t) =
  t.subscribers <- Map.remove t.subscribers subscription.id;
  update_write t;
;;
