open Int_replace_polymorphic_compare
open Sexplib.Std
module Sexp = Sexplib.Sexp

module List  = Core_list0
module Array = Caml.ArrayLabels


module Pre_float : Polymorphic_compare_intf.Infix with type t = float = struct
  type t = float

  open Pervasives

  let ( >= ) (x : t) (y : t) = (x >= y)
  let ( <= ) (x : t) (y : t) = (x <= y)
  let ( =  ) (x : t) (y : t) = (x =  y)
  let ( >  ) (x : t) (y : t) = (x >  y)
  let ( <  ) (x : t) (y : t) = (x <  y)
  let ( <> ) (x : t) (y : t) = (x <> y)
end

module Pre_int : Quickcheck_intf.Pre_int with type t = int = struct
  type t = int [@@deriving sexp, compare]
  include Int_replace_polymorphic_compare
  let num_bits    = Word_size.num_bits Word_size.word_size - 1
  let (+)         = (+)
  let (-)         = (-)
  let (~-)        = (~-)
  let zero        = 0
  let one         = 1
  let min_value   = min_int
  let max_value   = max_int
  let abs         = abs
  let succ        = succ
  let bit_not     = (lnot)
  let bit_and     = (land)
  let shift_left  = (lsl)
  let shift_right = (asr)
  let of_int_exn  = Fn.id
  let to_int_exn  = Fn.id
  let to_float    = float_of_int
end

let count_bits_non_negative
      (type t)
      (module I : Quickcheck_intf.Pre_int with type t = t)
      (t : t)
  =
  let open I in
  if t < zero
  then failwith "count_bits_non_negative: input is negative";
  let n = ref 0 in
  let t = ref t in
  while !t > zero do
    Pervasives.incr n;
    t := shift_right !t 1
  done;
  !n

let%test_module "count_bits_non_negative" =
  (module struct
    let check x num_bits =
      [%test_result: int]
        (count_bits_non_negative (module Pre_int) x)
        ~expect:num_bits
        ~message:(Printf.sprintf "bits of %d" x)

    let%test_unit _ = check 4 3
    let%test_unit _ = check 5 3
    let%test_unit _ = check 8 4
    let%test_unit "log based" =
      let lg x = log x /. log 2. in
      let expect n = Pervasives.succ (int_of_float (lg (float_of_int n))) in
      for i = 1 to 10_000 do
        check i (expect i)
      done
    ;;
  end)

module Raw_generator = struct

  let module_name = "Quickcheck_kernel.Std.Generator"

  module Approx = struct

    let compare x y =
      let diff = x -. y in
      if Pre_float.( < ) diff (-. Pervasives.epsilon_float) then -1 else
      if Pre_float.( > ) diff     Pervasives.epsilon_float  then  1 else
        0

    let ( =. ) x y = compare x y = 0

  end

  let float_is_finite float =
    match Pervasives.classify_float float with
    | FP_normal   | FP_subnormal | FP_zero -> true
    | FP_infinite | FP_nan                 -> false

  (* [residual] represents a portion of a generator's probability distribution that
     remains after some of it has been "used up" in a sense.  A [residual] may be:

     [Keep], meaning the whole generator remains and none has been used up.

     [Drop], meaning none of the generator remains and all of it has been used up.

     [Part (p, w, r1, r2)], meaning part of a weighted-choice generator has been used up.
     [p] is the portion (out of 1.0) of the probability distribution that remains, [w] is
     the weight (out of 1.0) of the left branch of the original weighted choice, and [r1]
     and [r2] are the residuals of the left and right branches of the weighted choice,
     respectively.

     Invariant: [Part (_, _, Keep, Keep)] and [Part (_, _, Drop, Drop)] should never be
     created; they should always be just [Keep] and [Drop] respectively.
  *)
  type residual =
    | Keep
    | Drop
    | Part of float * float * residual * residual

  (* ['a t] represents a probability distribution of values of type ['a].

     [Failure] means an empty distribution with no values (guaranteed to fail to produce a
     value).

     [Singleton x] means a distribution with a 100% chance to produce [x].

     [Weighted (w, t1, t2)] means a distribution that recursively produces values from
     [t1] with probability [w] and from [t2] with probability [1. -. w].

     [Of_fun (r, f)] means a distribution that recursively produces values from the
     remainder of the generator [f ()] according to the residual [r].  Keeping residuals
     in the [Of_fun] constructor allows residuals to be applied to generators without
     caching the result of applying [f].  *)
  type 'a t =
    | Failure
    | Singleton of 'a
    | Weighted  of float * 'a t * 'a t
    | Of_fun    of residual * (unit -> 'a t)

  let failure = Failure
  let singleton x = Singleton x
  let of_fun f = Of_fun (Keep, f)

  let remaining_fraction_of_residual r =
    match r with
    | Keep -> 1.
    | Drop -> 0.
    | Part (p, _, _, _) -> p

  let partial ~original_weight_of_left r1 r2 =
    match r1, r2 with
    | Keep, Keep -> Keep
    | Drop, Drop -> Drop
    | _ ->
      let p1 = remaining_fraction_of_residual r1 in
      let p2 = remaining_fraction_of_residual r2 in
      let w1 = p1 *.        original_weight_of_left  in
      let w2 = p2 *. (1. -. original_weight_of_left) in
      let p = w1 +. w2 in
      Part (p, original_weight_of_left, r1, r2)

  let mismatch_error ~from_str ~to_str () =
    failwith
      (Printf.sprintf
         "%s: %s changed to %s, possibly due to nondeterministic [of_fun]"
         module_name
         from_str
         to_str)

  (* [compose_residuals ~outer ~inner] computes the residual that is equivalent to [outer]
     composed with [inner].  This is used to accumulate residuals in [Of_fun] generators.
     The intended invariant is:

     {[
       apply_residual (compose_residuals ~outer ~inner) t
       =
       apply_residual outer (apply_residual inner t)
     ]}

     See immediately below for [apply_residual].
     See test module at the bottom of the file for tests of the invariant.
  *)
  let rec compose_residuals ~outer ~inner =
    match outer, inner with

    (* Simple base cases. *)
    | Drop, _ | _, Drop -> Drop
    | Keep, r | r, Keep -> r

    (* When the inner residual drops half of a weighted choice, it will simply promote the
       other branch rather than re-create the choice.  To account for that, we push the
       outer residual into the branch that will be kept. *)
    | outer, Part (_, original_weight_of_left, inner, Drop) ->
      partial ~original_weight_of_left
        (compose_residuals ~outer ~inner)
        Drop
    | outer, Part (_, original_weight_of_left, Drop, inner) ->
      partial ~original_weight_of_left
        Drop
        (compose_residuals ~outer ~inner)

    (* Otherwise, recursively combine both branches. *)
    | Part (_, _, r1_o, r2_o), Part (_, original_weight_of_left, r1_i, r2_i) ->
      partial ~original_weight_of_left
        (compose_residuals ~outer:r1_o ~inner:r1_i)
        (compose_residuals ~outer:r2_o ~inner:r2_i)
  ;;

  (* [apply_residual r t] produces the remainder of [t] according to [r]. *)
  let rec apply_residual r t =
    match r, t with

    (* Simple base cases. *)
    | Drop, _ -> Failure
    | Keep, _ -> t

    (* Accumulate residuals at [Of_fun]. *)
    | _, Of_fun (r', f) -> Of_fun (compose_residuals ~outer:r ~inner:r', f)

    (* Error on mismatched residuals. *)
    | Part _, Failure     -> mismatch_error ~from_str:"Failure"   ~to_str:"Part" ()
    | Part _, Singleton _ -> mismatch_error ~from_str:"Singleton" ~to_str:"Part" ()

    (* Dropping half of a weighted choice no longer requires a choice. *)
    | Part (_, _, Drop, r), Weighted (_, _, t) -> apply_residual r t
    | Part (_, _, r, Drop), Weighted (_, t, _) -> apply_residual r t

    (* Otherwise, recur on choices and recompute the weight. *)
    | Part (_, original_weight, r1, r2), Weighted (w, t1, t2) ->
      if not (Approx.( =. ) original_weight w) then
        mismatch_error ()
          ~from_str:(Pervasives.string_of_float w)
          ~to_str:(Pervasives.string_of_float original_weight);
      let w1 = (remaining_fraction_of_residual r1) *. w in
      let w2 = (remaining_fraction_of_residual r2) *. (1. -. w) in
      Weighted (w1 /. (w1 +. w2), apply_residual r1 t1, apply_residual r2 t2)
  ;;

  let rec apply_fun r f =
    match f () with
    | Of_fun (r', f') -> apply_fun (compose_residuals ~outer:r ~inner:r') f'
    | t               -> apply_residual r t

  let rec bind t t_of_a =
    of_fun (fun () ->
      match t with
      | Failure              -> Failure
      | Singleton a          -> t_of_a a
      | Weighted (w, t1, t2) -> Weighted (w, bind t1 t_of_a, bind t2 t_of_a)
      | Of_fun (r, f)        -> bind (apply_fun r f) t_of_a)

  let rec rev_app_weighted_union_of_neighbors alist acc =
    match alist with
    | [] -> acc
    | [ pair ] -> pair :: acc
    | (w1, t1) :: (w2, t2) :: alist ->
      let w = w1 +. w2 in
      let t = Weighted (w1 /. w, t1, t2) in
      rev_app_weighted_union_of_neighbors alist ((w, t) :: acc)

  let weighted_union_of_neighbors alist =
    List.rev (rev_app_weighted_union_of_neighbors alist [])

  let rec unchecked_weighted_union alist =
    match alist with
    | []         -> Failure
    | [ (_, t) ] -> t
    | _          -> unchecked_weighted_union (weighted_union_of_neighbors alist)

  let check_weight w =
    if Pre_float.( < ) w 0.
    then failwith "Generator.weighted_union: weight is negative";
    if not (float_is_finite w)
    then failwith "Generator.weighted_union: weight is infinite or NaN"

  let weighted_union alist =
    unchecked_weighted_union (List.filter alist ~f:(fun (w, _) ->
      check_weight w;
      Pre_float.( <> ) w 0.))

  let%test _ =
    Pervasives.(=) (weighted_union [ 0., Singleton 1 ; 0., Singleton 2 ]) Failure
  let%test _ =
    Pervasives.(=) (weighted_union [ 0., Singleton 1 ; 1., Singleton 2 ]) (Singleton 2)

  let rec inspect_weighted w t acc =
    match t with
    | Weighted (w', t1, t2) ->
      inspect_weighted (w *. w') t1
        (inspect_weighted (w *. (1. -. w')) t2 acc)
    | _ -> (w, t) :: acc

  let rec inspect t =
    match t with
    | Failure       -> `Failure
    | Singleton x   -> `Singleton x
    | Of_fun (r, f) -> inspect (apply_fun r f)
    | _             -> `Weighted_union (inspect_weighted 1. t [])

  module Attempt : sig
    type 'a gen = 'a t
    type 'a t
    val create : 'a gen -> 'a t
    val attempts_used : 'a t -> int
    val probability : 'a t -> float

    val at : 'a t -> 'a gen
    val original_gen : 'a t -> 'a gen
    val updated_gen
      : 'a t
      -> keep:[ `All_choices
              | `All_choices_except_this_choice
              | `Choices_to_the_left_of_this_choice_only
              | `Choices_to_the_right_of_this_choice_only
              | `This_choice_and_all_choices_to_the_left
              | `This_choice_and_all_choices_to_the_right
              ]
      -> 'a gen

    val left_after  : 'a t -> original_weight_of_left:float -> 'a t
    val right_after : 'a t -> original_weight_of_left:float -> 'a t
    val rewind : 'a t -> 'a t option
  end = struct

    type 'a gen = 'a t

    type dir  = [ `left | `right ]
    type step =
      { dir                       : dir
      ; already_failed_other_side : bool
      ; original_weight_of_left   : float
      }

    type 'a t =
      { original_gen  : 'a gen
      ; rev_path      : step list
      ; attempts_used : int
      } [@@deriving fields]

    let probability_of_step step =
      match step.dir with
      | `left  ->       step.original_weight_of_left
      | `right -> 1. -. step.original_weight_of_left

    let probability t =
      List.fold t.rev_path ~init:1. ~f:(fun prob step ->
        prob *. probability_of_step step)

    let rec go dir gen =
      match gen with
      | (Failure | Singleton _) -> assert false
      | Of_fun   (r, f)         -> go dir (apply_fun r f)
      | Weighted (_, t1, t2)    ->
        match dir with
        | `left  -> t1
        | `right -> t2

    let at_path gen ~rev_path =
      List.fold_right rev_path ~init:gen ~f:(fun step gen ->
        go step.dir gen)

    let at t =
      at_path t.original_gen ~rev_path:t.rev_path

    let create original_gen =
      { original_gen
      ; rev_path      = []
      ; attempts_used = 1
      }

    let left_after t ~original_weight_of_left =
      let step =
        { original_weight_of_left
        ; dir                       = `left
        ; already_failed_other_side = false
        }
      in
      { t with rev_path = step :: t.rev_path }

    let right_after t ~original_weight_of_left =
      let step =
        { original_weight_of_left
        ; dir                       = `right
        ; already_failed_other_side = false
        }
      in
      { t with rev_path = step :: t.rev_path }

    let residual_for ~rev_path ~left ~self ~right =
      List.fold rev_path ~init:self ~f:(fun r step ->
        let { dir ; original_weight_of_left ; already_failed_other_side } = step in
        let r1, r2 =
          match dir with
          | `left  ->
            if already_failed_other_side
            then r, Drop
            else r, right
          | `right ->
            if already_failed_other_side
            then Drop, r
            else left, r
        in
        partial ~original_weight_of_left r1 r2)

    let updated_gen t ~keep =
      let left, self, right =
        match keep with
        | `All_choices                              -> Keep, Keep, Keep
        | `All_choices_except_this_choice           -> Keep, Drop, Keep
        | `Choices_to_the_left_of_this_choice_only  -> Keep, Drop, Drop
        | `Choices_to_the_right_of_this_choice_only -> Drop, Drop, Keep
        | `This_choice_and_all_choices_to_the_left  -> Keep, Keep, Drop
        | `This_choice_and_all_choices_to_the_right -> Drop, Keep, Keep
      in
      let residual = residual_for ~rev_path:t.rev_path ~left ~self ~right in
      apply_residual residual t.original_gen

    let flip_dir = function
      | `right -> `left
      | `left  -> `right

    let rec rewind_path ~rev_path =
      match rev_path with
      | []               -> None
      | step :: rev_path ->
        if step.already_failed_other_side
        then rewind_path ~rev_path
        else
          let step' =
            { step with
              dir                       = flip_dir step.dir
            ; already_failed_other_side = true
            }
          in
          Some (step' :: rev_path)

    let rewind t =
      match rewind_path ~rev_path:t.rev_path with
      | None          -> None
      | Some rev_path -> Some { t with rev_path ; attempts_used = t.attempts_used + 1 }

  end

  module Choice = struct

    type 'a t =
      { attempt : 'a Attempt.t
      ; value   : 'a
      } [@@deriving fields]

    let create ~attempt ~value =
      { attempt ; value }

    let updated_gen t ~keep =
      Attempt.updated_gen t.attempt ~keep

    let attempts_used t = Attempt.attempts_used t.attempt
    let original_gen  t = Attempt.original_gen  t.attempt
    let probability   t = Attempt.probability   t.attempt

  end

  let rec choices t ~attempt =
    of_fun (fun () ->
      match t with
      | Failure          -> Failure
      | Singleton value  -> Singleton (Choice.create ~attempt ~value)
      | Of_fun    (r, f) -> choices (apply_fun r f) ~attempt
      | Weighted (original_weight_of_left, x, y) ->
        let x' =
          choices x ~attempt:(Attempt.left_after attempt ~original_weight_of_left)
        in
        let y' =
          choices y ~attempt:(Attempt.right_after attempt ~original_weight_of_left)
        in
        Weighted (original_weight_of_left, x', y'))

  let bind_choice t f =
    bind (choices t ~attempt:(Attempt.create t)) f

  let rec choose_from t ~attempt ~max_attempts ~random_float =
    if Attempt.attempts_used attempt > max_attempts then `Ran_out_of_attempts else
      match t with
      | Singleton value -> `Choice { Choice. attempt ; value }
      | Of_fun (r, t)   ->
        choose_from (apply_fun r t) ~attempt ~max_attempts ~random_float
      | Weighted (original_weight_of_left, t1, t2) ->
        if Pre_float.( < ) (random_float ()) original_weight_of_left
        then
          let attempt = Attempt.left_after attempt ~original_weight_of_left in
          choose_from t1 ~attempt ~max_attempts ~random_float
        else
          let attempt = Attempt.right_after attempt ~original_weight_of_left in
          choose_from t2 ~attempt ~max_attempts ~random_float
      | Failure ->
        match Attempt.rewind attempt with
        | None         -> `No_choices_remain
        | Some attempt ->
          let t = Attempt.at attempt in
          choose_from t ~attempt ~max_attempts ~random_float

  (* [choose] is implemented in terms of [choose_residual] and [Choice] so that, as
     subtrees are eliminated from the generator, only residuals are accumulated.
     Specifically, the results of [apply_fun] are never cached. *)
  let choose t ~random_float_between_zero_and_one:random_float ~max_attempts =
    choose_from t ~max_attempts ~attempt:(Attempt.create t) ~random_float

  let%test_module "raw generator" =
    (module struct

      (* This registers a top-level exception printer that uses sexp conversions. *)
      let () =
        Printexc.register_printer (fun exc ->
          match Sexplib.Conv.sexp_of_exn_opt exc with
          | None -> None
          | Some sexp ->
            Some (Sexp.to_string_hum ~indent:2 sexp))

      type approx_float = float [@@deriving sexp]

      let compare_approx_float = Approx.compare

      type approx_residual =
        [ `Keep
        | `Drop
        | `Part of approx_float * approx_float * approx_residual * approx_residual
        ] [@@deriving sexp, compare]

      type 'a approx_t =
        [ `Failure
        | `Singleton of 'a
        | `Of_fun of approx_residual * 'a approx_t
        | `Weighted of approx_float * 'a approx_t * 'a approx_t
        ] [@@deriving sexp, compare]

      let rec approximate_residual r =
        match r with
        | Keep -> `Keep
        | Drop -> `Drop
        | Part (p, w, r1, r2) ->
          `Part (p, w, approximate_residual r1, approximate_residual r2)

      let rec approximate_t t =
        match t with
        | Failure               -> `Failure
        | Singleton a           -> `Singleton a
        | Of_fun    (r, f)      -> `Of_fun (approximate_residual r, approximate_t (f ()))
        | Weighted  (w, t1, t2) -> `Weighted (w, approximate_t t1, approximate_t t2)

      let sexp_of_residual r = [%sexp_of: approx_residual] (approximate_residual r)
      let sexp_of_t sexp_of_a t = [%sexp_of: a approx_t] (approximate_t t)

      let rec residuals_of_t t =
        match t with
        | Failure     -> [ Keep ; Drop ]
        | Singleton _ -> [ Keep ; Drop ]
        | Of_fun (r, t) -> residuals_of_t (apply_fun r t)
        | Weighted (original_weight_of_left, t1, t2) ->
          let residuals1 = residuals_of_t t1 in
          let residuals2 = residuals_of_t t2 in
          List.concat_map residuals1 ~f:(fun r1 ->
            List.map residuals2 ~f:(fun r2 ->
              partial ~original_weight_of_left r1 r2))

      let rec make_sub_int_t ~lower_inclusive ~upper_exclusive =
        if lower_inclusive >= upper_exclusive
        then Failure
        else if lower_inclusive = upper_exclusive - 1
        then Singleton lower_inclusive
        else
          let middle = (lower_inclusive + upper_exclusive) / 2 in
          let w =
            float (middle          - lower_inclusive) /.
            float (upper_exclusive - lower_inclusive)
          in
          let t1 =
            Of_fun (Keep, fun () ->
              make_sub_int_t ~lower_inclusive ~upper_exclusive:middle)
          in
          let t2 =
            Of_fun (Keep, fun () ->
              make_sub_int_t ~lower_inclusive:middle ~upper_exclusive)
          in
          Weighted (w, t1, t2)

      let make_int_t n = make_sub_int_t ~lower_inclusive:0 ~upper_exclusive:n

      let init n ~f =
        let rec loop i =
          if i = n
          then []
          else f i :: loop (i + 1)
        in
        loop 0

      let int_t_list = init 10 ~f:make_int_t

      let seed_arrays = init 10 ~f:(fun i -> Array.init i ~f:(fun x -> x))

      let rec to_list t =
        match t with
        | Failure              -> []
        | Singleton x          -> [x]
        | Of_fun (r, f)        -> to_list (apply_fun r f)
        | Weighted (_, t1, t2) -> to_list t1 @ to_list t2

      module Int_set = struct
        include Caml.Set.Make(struct
            type t = int
            let compare (x:int) y = Pervasives.compare x y
          end)
        let sexp_of_t t = [%sexp_of: int list] (elements t)
      end

      let to_set t = to_list t |> Int_set.of_list

      exception Quickcheck of string * Sexp.t * exn [@@deriving sexp]

      let with_note msg sexp_of_a a f =
        match f () with
        | x -> x
        | exception exn -> raise (Quickcheck (msg, sexp_of_a a, exn))

      let iter list msg sexp_of f =
        List.iter list ~f:(fun x ->
          with_note msg sexp_of x (fun () -> f x))

      let choose_by_state t random_state =
        let random_float_between_zero_and_one () =
          Random.State.float random_state 1.
        in
        choose t ~max_attempts:1 ~random_float_between_zero_and_one

      let rec choose_set_and_test t random_state =
        match choose_by_state t random_state with
        | `Ran_out_of_attempts -> assert false
        | `No_choices_remain   ->
          [%test_result: Int_set.t] (to_set t) ~expect:Int_set.empty;
          Int_set.empty
        | `Choice c ->
          let t' = Choice.updated_gen c ~keep:`All_choices_except_this_choice in
          let value = Choice.value c in
          [%test_result: Int_set.t] (to_set t') ~expect:(Int_set.remove value (to_set t));
          Int_set.add value (choose_set_and_test t' random_state)

      let%test_unit "apply_residual" =
        iter int_t_list "generator" [%sexp_of: int t] (fun t ->
          iter (residuals_of_t t) "residual" [%sexp_of: residual] (fun r ->
            let t' = apply_residual r t in
            assert (Int_set.subset (to_set t') (to_set t))))

      let%test_unit "compose_residuals" =
        iter int_t_list "original generator" [%sexp_of: int t] (fun t ->
          iter (residuals_of_t t) "inner residual" [%sexp_of: residual] (fun inner ->
            let t' = apply_residual inner t in
            with_note "intermediate generator" [%sexp_of: int t] t' (fun () ->
              iter (residuals_of_t t') "outer residual" [%sexp_of: residual] (fun outer ->
                [%test_eq: int approx_t]
                  (approximate_t (apply_residual outer (apply_residual inner t)))
                  (approximate_t (apply_residual (compose_residuals ~outer ~inner) t))))))

      let%test_unit "Choice.ne" =
        iter seed_arrays "seed" [%sexp_of: int array] (fun seed ->
          iter int_t_list "generator" [%sexp_of: int t] (fun t ->
            let random_state = Random.State.make seed in
            match choose_by_state t random_state with
            | `No_choices_remain   -> ()
            | `Ran_out_of_attempts -> assert false
            | `Choice c            ->
              [%test_result: Int_set.t]
                (to_set (Choice.updated_gen c ~keep:`All_choices_except_this_choice))
                ~expect:(Int_set.remove (Choice.value c) (to_set t))))

      let%test_unit "choose" =
        iter seed_arrays "seed" [%sexp_of: int array] (fun seed ->
          iter int_t_list "generator" [%sexp_of: int t] (fun t ->
            let random_state = Random.State.make seed in
            [%test_result: Int_set.t]
              (choose_set_and_test t random_state)
              ~expect:(to_set t)))

    end)

end

type 'a gen = 'a Raw_generator.t

module Raw_observer = struct

  module Sexpable_fn = struct

    type ('a, 'b) t = ('a -> 'b) * (unit -> Sexp.t)

    let create ~f ~f_sexp = f, f_sexp

    let const x ~sexp_of =
      create
        ~f:(fun _ -> x)
        ~f_sexp:(fun () ->
          [%sexp_of: [`const of Sexp.t]]
            (`const (sexp_of x)))

    let cases ~a:(f_a, mk_sexp_a) ~b:(f_b, mk_sexp_b) =
      create
        ~f:(function `A a -> f_a a | `B b -> f_b b)
        ~f_sexp:(fun () ->
          [%sexp_of: [`cases of [`A of Sexp.t] * [`B of Sexp.t]]]
            (`cases (`A (mk_sexp_a ()), `B (mk_sexp_b ()))))

    let compose ~fst:(f1, mk_sexp1) ~snd:(f2, mk_sexp2) =
      create
        ~f:(fun x -> f2 (f1 x))
        ~f_sexp:(fun () ->
          [%sexp_of: [`compose_left_to_right of Sexp.t * Sexp.t]]
            (`compose_left_to_right (mk_sexp1 (), mk_sexp2 ())))

    let duplicate () =
      create
        ~f:(fun x -> x, x)
        ~f_sexp:(fun () -> Sexp.Atom "duplicate")

    let lift_variant_from_fst () =
      create
        ~f:(function
          | (`A x), y -> `A (x, y)
          | (`B x), y -> `B (x, y))
        ~f_sexp:(fun () -> Sexp.Atom "lift_variant_from_fst")

    let lift_to_fst (f, mk_sexp) =
      create
        ~f:(fun (x, y) -> (f x, y))
        ~f_sexp:(fun () ->
          [%sexp_of: [`lift_to_fst of Sexp.t]]
            (`lift_to_fst (mk_sexp ())))

  end

  (* [t] represents a family of decision trees used to observe properties of some input
     type.  The [observe] function randomly chooses a single decision tree from the family
     represented by a [t].  It does so by choosing a subset of the available observations
     on a type, limiting the size of the final tree to a specific number of nodes.

     [Singleton] is a leaf in the decision tree; it makes no further decisions.

     [Variant2 (t_a, t_b)] is a node in the decision tree.  It distinguishes between [`A
     a] and [`B b].  Further observations are made using [t_a] to observe [a] or [t_b] to
     observe [b].

     [Random alist] combines multiple decision trees nondeterministically.  When
     constructing the combined decision tree, individual decision tree nodes are chosen
     from [alist] with probability proportional to their given weights.  Invariant:
     [alist] never includes [Singleton].

     [Unmap (t, fn)] wraps a node in a decision tree with intermediate work, such as
     mapping a value to a variant for use with [Variant2].  [fn] performs the conversion
     and [t] observes the output of that conversion.  Invariant: [t] is never [Singleton].

     [Fn (p, gen, t, sexp_of)] produces decision tree nodes for observing a function by
     randomly generating inputs for the function from [gen] and then observing the
     function's output using [t].  If previous outputs have been generated, [observe]
     chooses a new input with probability [p] and a previous input with probability [1-p].
     Each input can be rendered using [sexp_of].  Invariant: [t] is never [Singleton].

     [Of_fun f] lazily produces a [t]; it is generally used to short-circuit values that
     may be infinite or intractably large so that they can be explored on demand. *)
  type 'a t =
    | Singleton : _ t
    | Variant2  : 'a t * 'b t                            -> [ `A of 'a | `B of 'b ] t
    | Random    : (float * 'a t) list                    -> 'a t
    | Unmap     : 'b t * ('a, 'b) Sexpable_fn.t          -> 'a t
    | Fn        : float * 'a gen * 'b t * ('a -> Sexp.t) -> ('a -> 'b) t
    | Of_fun    : (unit -> 'a t)                         -> 'a t

  let rec limited_branching_factor
    : type a . a t -> limit:int -> int
    = fun t ~limit ->
      match t with
      | Singleton           -> 0
      | Unmap (t, _)        -> limited_branching_factor t ~limit
      | Variant2 (t_a, t_b) ->
        let x = limited_branching_factor t_a ~limit in
        let y = limited_branching_factor t_b ~limit in
        min limit (x + y + 1)
      | Random alist ->
        List.fold alist ~init:0 ~f:(fun x (_, t_b) ->
          let y = limited_branching_factor t_b ~limit in
          min limit (((x + 1) * (y + 1)) - 1))
      | Fn _ ->
        (* We don't know the "size" of generators, and exponential spaces are large, so
           assume functions have a large branching factor. *)
        limit
      | Of_fun _ ->
        (* We don't want to unroll a potentially infinite space, and of_fun is generally
           used for infinite or intractably large spaces, so assume lazy observers have a
           large branching factor. *)
        limit

  let max_branching_factor = (1 lsl 15) - 1

  let branching_factor t = limited_branching_factor t ~limit:max_branching_factor

  let singleton () = Singleton
  let variant2 t_a t_b = Variant2 (t_a, t_b)

  let check_weight wt =
    match Pervasives.classify_float wt with
    | FP_nan | FP_infinite ->
      failwith "Observer.weighted_union: weight is not finite"
    | _ ->
      if Pre_float.( < ) wt 0. then
        failwith "Observer.weighted_union: weight is negative"

  let is_singleton = function Singleton -> true | _ -> false

  let weighted_union alist =
    let filtered_alist =
      List.filter alist ~f:(fun (wt, t) ->
        check_weight wt;
        not (is_singleton t))
    in
    match filtered_alist with
    | []         -> Singleton
    | [ (_, t) ] -> t
    | alist      -> Random alist

  let unmap t ~f ~f_sexp =
    match t with
    | Singleton -> Singleton
    | _         -> Unmap (t, Sexpable_fn.create ~f ~f_sexp)

  let of_fun f = Of_fun f

  let fn ?(p = 0.25) dom_gen rng_t ~sexp_of_dom =
    match rng_t with
    | Singleton -> Singleton
    | _         -> Fn (p, dom_gen, rng_t, sexp_of_dom)

  let unmap_fst t = unmap t ~f:fst ~f_sexp:(fun () -> Sexp.Atom "fst")
  let unmap_snd t = unmap t ~f:snd ~f_sexp:(fun () -> Sexp.Atom "snd")

  let tuple2 a b =
    weighted_union
      [ 1., unmap_fst a
      ; 1., unmap_snd b
      ]

  let tuple3 a b c =
    weighted_union
      [ 1., unmap a ~f:(fun (x, _, _) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_1st_of_3")
      ; 1., unmap b ~f:(fun (_, x, _) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_2nd_of_3")
      ; 1., unmap c ~f:(fun (_, _, x) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_3rd_of_3")
      ]

  let tuple4 a b c d =
    weighted_union
      [ 1., unmap a ~f:(fun (x,_,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_1st_of_4")
      ; 1., unmap b ~f:(fun (_,x,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_2nd_of_4")
      ; 1., unmap c ~f:(fun (_,_,x,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_3rd_of_4")
      ; 1., unmap d ~f:(fun (_,_,_,x) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_4th_of_4")
      ]

  let tuple5 a b c d e =
    weighted_union
      [ 1., unmap a ~f:(fun (x,_,_,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_1st_of_5")
      ; 1., unmap b ~f:(fun (_,x,_,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_2nd_of_5")
      ; 1., unmap c ~f:(fun (_,_,x,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_3rd_of_5")
      ; 1., unmap d ~f:(fun (_,_,_,x,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_4th_of_5")
      ; 1., unmap e ~f:(fun (_,_,_,_,x) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_5th_of_5")
      ]

  let tuple6 a b c d e f =
    weighted_union
      [ 1., unmap a ~f:(fun (x,_,_,_,_,_) -> x)
              ~f_sexp:(fun () -> Sexp.Atom "get_1st_of_6")
      ; 1., unmap b ~f:(fun (_,x,_,_,_,_) -> x)
              ~f_sexp:(fun () -> Sexp.Atom "get_2nd_of_6")
      ; 1., unmap c ~f:(fun (_,_,x,_,_,_) -> x)
              ~f_sexp:(fun () -> Sexp.Atom "get_3rd_of_6")
      ; 1., unmap d ~f:(fun (_,_,_,x,_,_) -> x)
              ~f_sexp:(fun () -> Sexp.Atom "get_4th_of_6")
      ; 1., unmap e ~f:(fun (_,_,_,_,x,_) -> x)
              ~f_sexp:(fun () -> Sexp.Atom "get_5th_of_6")
      ; 1., unmap f ~f:(fun (_,_,_,_,_,x) -> x)
              ~f_sexp:(fun () -> Sexp.Atom "get_6th_of_6")
      ]

  (* [gen_uniform_between ~lo ~hi] produces numbers uniformly distributed between [lo] and
     [hi], both inclusive.  It handles natural numbers up to [max_branching_factor], and
     thus does not need to worry about overflow in arithmetic. *)
  let rec gen_uniform_between ~lo ~hi =
    if lo > hi then Raw_generator.failure      else
    if lo = hi then Raw_generator.singleton lo else
      begin
        let lo_mid = (lo + hi) / 2 in
        let hi_mid = lo_mid + 1    in
        Raw_generator.weighted_union
          [ Pervasives.float_of_int (1 + lo_mid - lo),
            Raw_generator.of_fun (fun () -> gen_uniform_between ~lo ~hi:lo_mid)
          ; Pervasives.float_of_int (1 + hi - hi_mid),
            Raw_generator.of_fun (fun () -> gen_uniform_between ~lo:hi_mid ~hi)
          ]
      end

  (* [gen_ways_to_split n ~limit_a ~limit_b] produces pairs of natural numbers [a, b] such
     that [a + b = n], [a <= limit_a], and [b <= limit_b].  It handles natural numbers up
     to [max_branching_factor], and thus does not need to worry about overflow in
     arithmetic. *)
  let gen_ways_to_split n ~limit_a ~limit_b =
    Raw_generator.bind (gen_uniform_between ~lo:(n - limit_b) ~hi:limit_a) (fun a ->
      Raw_generator.singleton (a, n-a))

  (* [decision] represents a single node in a decision tree, along with [t]s representing
     the space of possible children.

     [Decide (t_a, t_b)] represents a node that distinguishes between [`A a] and [`B b],
     with child nodes chosen from [t_a] to observe [a] and child nodes chosen from [t_b]
     to observe [b].

     [Apply (fn, decision)] represents a "wrapper" for a node that first applies [fn] to
     an input, and then uses [decision] to observe the result. *)
  type 'a decision =
    | Decide : 'a t * 'b t                          -> [ `A of 'a | `B of 'b ] decision
    | Apply  : ('a, 'b) Sexpable_fn.t * 'b decision -> 'a decision

  (* [randomize_decision decision i alist] is used when [decision] was chosen as one
     possible decision tree node from index [i] of [alist], which is a list of [t]s with
     [float] weights.  This function re-wraps the nondeterministic choice of [alist]
     around the child nodes of [decision], substituting the child nodes of [decision] for
     element [i] of [alist]. *)
  let randomize_decision (type dom) decision i alist =
    let rec loop
      : type typ . typ decision -> (typ * dom) decision
      = function
        | Apply (fn, decision) ->
          Apply (Sexpable_fn.lift_to_fst fn, loop decision)
        | Decide (t_a, t_b) ->
          let t_a' =
            weighted_union (List.mapi alist ~f:(fun j (wt, t) ->
              if j = i
              then wt, unmap_fst t_a
              else wt, unmap_snd t))
          in
          let t_b' =
            weighted_union (List.mapi alist ~f:(fun j (wt, t) ->
              if j = i
              then wt, unmap_fst t_b
              else wt, unmap_snd t))
          in
          Apply (Sexpable_fn.lift_variant_from_fst (), Decide (t_a', t_b'))
    in
    Apply (Sexpable_fn.duplicate (), loop decision)

  (* [decide t] produces a random generator of all initial [`A _]-versus-[`B _] decisions
     that [t] can make. *)
  let rec decide : type dom . dom t -> dom decision Raw_generator.t = function
    | Of_fun f            -> decide (f ())
    | Singleton           -> Raw_generator.failure
    | Variant2 (t_a, t_b) -> Raw_generator.singleton (Decide (t_a, t_b))
    | Unmap (t, fn) ->
      Raw_generator.bind (decide t) (fun decision ->
        Raw_generator.singleton (Apply (fn, decision)))
    | Random alist ->
      let gen =
        Raw_generator.weighted_union
          (List.mapi alist ~f:(fun i (wt, t) ->
             (wt, Raw_generator.singleton (i, t))))
      in
      Raw_generator.bind gen (fun (i, t) ->
        Raw_generator.bind (decide t) (fun decision ->
          Raw_generator.singleton (randomize_decision decision i alist)))
    | Fn (p, gen, t, sexp_of) ->
      Raw_generator.bind_choice gen (fun choice ->
        (* We don't want to repeat this choice of values again, so we strip out this
           choice of input from [gen].  We do want to allow different orders of the same
           inputs, however; see comment in .mli about "intensionally unique" functions. *)
        let dom  = Raw_generator.Choice.value choice in
        let gen' =
          Raw_generator.Choice.updated_gen choice ~keep:`All_choices_except_this_choice
        in
        let t' =
          unmap t ~f:(fun f -> f dom)
            ~f_sexp:(fun () ->
              [%sexp_of: [`apply_to of Sexp.t]]
                (`apply_to (sexp_of dom)))
        in
        let alist =
          [ 1. -. p, t'
          ;       p, Fn (p, gen', t, sexp_of)
          ]
        in
        Raw_generator.bind (decide t') (fun decision ->
          Raw_generator.singleton (randomize_decision decision 0 alist)))

  module type S = sig
    type t [@@deriving sexp_of]
    val gen : t Raw_generator.t
  end

  module Make (T : S) : sig

    val fn_gen_of_t
      :  'a t
      -> branching_factor:int
      -> ('a, T.t) Sexpable_fn.t Raw_generator.t

  end = struct

    let rec fn_gen_of_t
      : type dom .
        (dom t -> branching_factor : int -> (dom, T.t) Sexpable_fn.t Raw_generator.t)
      = fun t ~branching_factor ->
        if branching_factor = 0
        then
          Raw_generator.bind T.gen (fun x ->
            Raw_generator.singleton (Sexpable_fn.const x ~sexp_of:[%sexp_of: T.t]))
        else
          Raw_generator.bind (decide t) (fun decision ->
            fn_gen_of_decision decision ~branching_factor)

    and fn_gen_of_decision
      : type dom .
        (dom decision
         -> branching_factor : int
         -> (dom, T.t) Sexpable_fn.t Raw_generator.t)
      = fun decision ~branching_factor ->
        match decision with
        | Apply (fn1, decision) ->
          Raw_generator.bind (fn_gen_of_decision decision ~branching_factor) (fun fn2 ->
            Raw_generator.singleton (Sexpable_fn.compose ~fst:fn1 ~snd:fn2))
        | Decide (t_a, t_b) ->
          let limit = branching_factor - 1 in
          let limit_a = limited_branching_factor t_a ~limit in
          let limit_b = limited_branching_factor t_b ~limit in
          Raw_generator.bind (gen_ways_to_split limit ~limit_a ~limit_b)
            (fun (n_a, n_b) ->
               Raw_generator.bind (fn_gen_of_t t_a ~branching_factor:n_a)
                 (fun fn_a ->
                    Raw_generator.bind (fn_gen_of_t t_b ~branching_factor:n_b)
                      (fun fn_b ->
                         Raw_generator.singleton (Sexpable_fn.cases ~a:fn_a ~b:fn_b))))

  end

  let observe (type rng) t gen ~sexp_of_rng ~branching_factor =
    if branching_factor < 0
    then failwith "Observer.observe: negative branching factor"
    else if branching_factor > max_branching_factor
    then Raw_generator.failure
    else
      let module T = struct type t = rng [@@deriving sexp_of] let gen = gen end in
      let module M = Make (T) in
      M.fn_gen_of_t t ~branching_factor

end

type 'a obs = 'a Raw_observer.t

module Generator = struct

  include Raw_generator
  include Monad.Make (struct
      include Raw_generator
      let return = singleton
      let map = `Define_using_bind
    end)

  type ('a, 'b) fn_with_sexp = ('a -> 'b) * (unit -> Sexp.t)

  let fn_sexp (_, mk_sexp) = mk_sexp ()

  let sexp_of_fn_with_sexp _ _ fn = fn_sexp fn

  let filter_map t ~f =
    t >>= fun x ->
    match f x with
    | Some x -> return x
    | None   -> failure

  let filter t ~f =
    t >>= fun x ->
    if f x
    then return x
    else failure

  let union ts =
    weighted_union (List.map ts ~f:(fun t -> (1., t)))

  let doubleton x y = union [ singleton x ; singleton y ]

  let of_list list = union (List.map list ~f:singleton)

  let of_sequence ?(p = 0.25) seq =
    if Pervasives.( < ) p 0. || Pervasives.( > ) p 1. then
      failwith (Printf.sprintf "Generator.of_sequence: probability [%f] out of bounds" p);
    Sequence.delayed_fold seq
      ~init:()
      ~finish:(fun () -> failure)
      ~f:(fun () x ~k ->
        weighted_union
          [       p, singleton x
          ; 1. -. p, of_fun k
          ])

  let geometric ?p ?maximum () =
    let stop =
      match maximum with
      | None   -> (fun n -> n < 0)
      | Some m -> (fun n -> n < 0 || n > m)
    in
    of_sequence ?p
      (Sequence.unfold ~init:0 ~f:(fun n ->
         if stop n
         then None
         else Some (n, n+1)))

  let size = geometric ()

  module Make_int_generator (M : Quickcheck_intf.Pre_int) : sig
    val gen : M.t t
    val gen_between
      :  lower_bound:M.t Maybe_bound.t
      -> upper_bound:M.t Maybe_bound.t
      -> M.t t
  end = struct

    open M

    let average_rounded_down x y =
      (shift_right x 1) + (shift_right y 1) + (bit_and (bit_and x y) one)

    let%test_unit "average_rounded_down" =
      let check here x y ~expect =
        let actual = average_rounded_down x y in
        [%test_result: M.t] ~here:[here] actual ~expect
      in
      let check_int here x y =
        check here (of_int_exn x) (of_int_exn y)
          ~expect:(of_int_exn Pervasives.((x + y) / 2))
      in
      check_int [%here] 3 5;
      check_int [%here] 4 6;
      check_int [%here] 4 5;
      check [%here] min_value max_value ~expect:(- one);
      check [%here] max_value min_value ~expect:(- one);
      check [%here] max_value max_value ~expect:max_value;
      check [%here] min_value min_value ~expect:min_value

    let rec lower_ranges ~lower_bound ~upper_bound =
      if lower_bound = upper_bound
      then [ (lower_bound, upper_bound) ]
      else
        let lower_middle = average_rounded_down lower_bound upper_bound in
        let upper_middle = succ lower_middle in
        (upper_middle, upper_bound) :: lower_ranges ~lower_bound ~upper_bound:lower_middle

    let upper_ranges ~lower_bound ~upper_bound =
      let flip x = upper_bound - x + lower_bound in
      List.map (lower_ranges ~lower_bound ~upper_bound) ~f:(fun (lower, upper) ->
        (flip upper, flip lower))

    let non_negative_ranges_by_magnitude ~lower_bound ~upper_bound =
      if lower_bound = upper_bound
      then [ (lower_bound, upper_bound) ]
      else
        let lower_middle = average_rounded_down lower_bound upper_bound in
        let upper_middle = succ lower_middle in
        lower_ranges ~lower_bound ~upper_bound:lower_middle @
        upper_ranges ~lower_bound:upper_middle ~upper_bound

    let negative_ranges_by_magnitude ~lower_bound ~upper_bound =
      List.map ~f:(fun (lower, upper) -> (bit_not upper, bit_not lower))
        (non_negative_ranges_by_magnitude
           ~lower_bound:(bit_not upper_bound)
           ~upper_bound:(bit_not lower_bound))

    let ranges_by_magnitude_and_sign ~lower_bound ~upper_bound =
      if lower_bound >= zero
      then non_negative_ranges_by_magnitude ~lower_bound ~upper_bound
      else if upper_bound < zero
      then negative_ranges_by_magnitude ~lower_bound ~upper_bound
      else
        negative_ranges_by_magnitude ~lower_bound ~upper_bound:(- one) @
        non_negative_ranges_by_magnitude ~lower_bound:zero ~upper_bound

    let%test_unit "ranges_by_magnitude_and_sign exhaustive" =
      let low = (-100) in
      let high = 100 in
      let lower_bound = of_int_exn low and upper_bound = of_int_exn high in
      let ranges =
        ranges_by_magnitude_and_sign ~lower_bound ~upper_bound
        |> List.map ~f:(fun (a, b) -> to_int_exn a, to_int_exn b)
      in
      let mem n =
        List.exists ranges ~f:(fun (lower, upper) ->
          Pervasives.(<=) lower n && Pervasives.(<=) n upper)
      in
      assert (not (mem (Pervasives.pred low)));
      assert (not (mem (Pervasives.succ high)));
      for i = low to high do assert (mem i) done
    ;;

    let%test_unit "ranges_by_magnitude_and_sign num_ranges grows slowly" =
      for i = 0 to Pervasives.(-) num_bits 2 do
        let n = shift_left one i in
        let num_ranges =
          ranges_by_magnitude_and_sign ~lower_bound:zero ~upper_bound:n
          |> List.length
        in
        assert (Pervasives.(num_ranges <= 2 * (i + 1)))
      done
    ;;

    let rec weighted_uniform ~lower_bound ~upper_bound =
      if lower_bound = upper_bound
      then 1., singleton lower_bound
      else
        ( to_float (succ (upper_bound - lower_bound))
        , of_fun (fun () ->
            let lower_middle = average_rounded_down lower_bound upper_bound in
            let upper_middle = succ lower_middle in
            weighted_union
              [ weighted_uniform ~lower_bound ~upper_bound:lower_middle
              ; weighted_uniform ~lower_bound:upper_middle ~upper_bound
              ])
        )

    let gen_uniform ~lower_bound ~upper_bound =
      snd (weighted_uniform ~lower_bound ~upper_bound)

    let gen_between_inclusive ~lower_bound ~upper_bound =
      if lower_bound > upper_bound
      then failure
      else
        (* [ranges] is a list of tuples representing inclusive lower and upper bounds of
           disjoint ranges that add up to the entirety of [lower_bound, upper_bound].
           These ranges are constructed to start at size 1 at the boundaries and
           approximately double in size as they approach the middle of the range.  Each
           range is converted into a uniform distribution of values.

           The final generator is constructed as a union of these ranges, weighted in
           inverse proportion to the log of their sizes.  The intention is to consistently
           exercise boundary conditions, while still leaving a fair probability of
           choosing arbitrary values out of the middle of the distribution. *)
        let ranges = ranges_by_magnitude_and_sign ~lower_bound ~upper_bound in
        weighted_union (List.map ranges ~f:(fun (lower, upper) ->
          let inverse_wt =
            count_bits_non_negative (module M) (succ (abs (upper - lower)))
          in
          1. /. Pervasives.float_of_int inverse_wt,
          gen_uniform ~lower_bound:lower ~upper_bound:upper))

    let gen_between ~lower_bound ~upper_bound =
      match
        (lower_bound : t Maybe_bound.t),
        (upper_bound : t Maybe_bound.t)
      with
      | Excl lower, _ when lower = max_value ->
        failwith "Int.gen_between: lower bound > max_value"
      | _, Excl upper when upper = min_value ->
        failwith "Int.gen_between: upper bound < min_value"
      | _ ->
        let lower_inclusive =
          match lower_bound with
          | Unbounded        -> min_value
          | Incl lower_bound -> lower_bound
          | Excl lower_bound -> lower_bound + one
        in
        let upper_inclusive =
          match upper_bound with
          | Unbounded        -> max_value
          | Incl upper_bound -> upper_bound
          | Excl upper_bound -> upper_bound - one
        in
        if lower_inclusive > upper_inclusive then
          Error.failwiths "Int.gen_between: bounds are crossed"
            (`lower_bound lower_bound, `upper_bound upper_bound)
            [%sexp_of: [`lower_bound of t Maybe_bound.t] *
                       [`upper_bound of t Maybe_bound.t]];
        gen_between_inclusive
          ~lower_bound:lower_inclusive
          ~upper_bound:upper_inclusive

    let gen =
      gen_between ~lower_bound:Unbounded ~upper_bound:Unbounded

  end

  module For_int = Make_int_generator (Pre_int)

  let recursive f =
    let rec self () = f (of_fun self) in
    of_fun self

  let variant2 a b =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ]

  let variant3 a b c =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ; map c ~f:(fun c -> `C c)
          ]

  let variant4 a b c d =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ; map c ~f:(fun c -> `C c)
          ; map d ~f:(fun d -> `D d)
          ]

  let variant5 a b c d e =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ; map c ~f:(fun c -> `C c)
          ; map d ~f:(fun d -> `D d)
          ; map e ~f:(fun e -> `E e)
          ]

  let variant6 a b c d e f =
    union [ map a ~f:(fun a -> `A a)
          ; map b ~f:(fun b -> `B b)
          ; map c ~f:(fun c -> `C c)
          ; map d ~f:(fun d -> `D d)
          ; map e ~f:(fun e -> `E e)
          ; map f ~f:(fun f -> `F f)
          ]

  let tuple2 t1 t2 =
    t1 >>= fun x1 ->
    t2 >>| fun x2 ->
    (x1, x2)

  let tuple3 t1 t2 t3 =
    t1 >>= fun x1 ->
    t2 >>= fun x2 ->
    t3 >>| fun x3 ->
    (x1, x2, x3)

  let tuple4 t1 t2 t3 t4 =
    t1 >>= fun x1 ->
    t2 >>= fun x2 ->
    t3 >>= fun x3 ->
    t4 >>| fun x4 ->
    (x1, x2, x3, x4)

  let tuple5 t1 t2 t3 t4 t5 =
    t1 >>= fun x1 ->
    t2 >>= fun x2 ->
    t3 >>= fun x3 ->
    t4 >>= fun x4 ->
    t5 >>| fun x5 ->
    (x1, x2, x3, x4, x5)

  let tuple6 t1 t2 t3 t4 t5 t6 =
    t1 >>= fun x1 ->
    t2 >>= fun x2 ->
    t3 >>= fun x3 ->
    t4 >>= fun x4 ->
    t5 >>= fun x5 ->
    t6 >>| fun x6 ->
    (x1, x2, x3, x4, x5, x6)

  let fn_with_sexp ?branching_factor dom rng ~sexp_of_rng =
    let branching_factor =
      match branching_factor with
      | Some t -> t
      | None   -> geometric () ~maximum:(Raw_observer.branching_factor dom)
    in
    branching_factor
    >>= fun branching_factor ->
    Raw_observer.observe ~branching_factor dom rng ~sexp_of_rng

  let fn2_with_sexp ?branching_factor dom1 dom2 rng ~sexp_of_rng =
    fn_with_sexp ?branching_factor
      (Raw_observer.tuple2 dom1 dom2)
      rng ~sexp_of_rng
    >>| fun (f, mk_sexp) ->
    (fun x1 x2 -> f (x1, x2)),
    (fun () -> [%sexp_of: [`curry of Sexp.t]] (`curry (mk_sexp ())))

  let fn3_with_sexp ?branching_factor dom1 dom2 dom3 rng ~sexp_of_rng =
    fn_with_sexp ?branching_factor
      (Raw_observer.tuple3 dom1 dom2 dom3)
      rng ~sexp_of_rng
    >>| fun (f, mk_sexp) ->
    (fun x1 x2 x3 -> f (x1, x2, x3)),
    (fun () -> [%sexp_of: [`curry3 of Sexp.t]] (`curry3 (mk_sexp ())))

  let fn4_with_sexp ?branching_factor dom1 dom2 dom3 dom4 rng ~sexp_of_rng =
    fn_with_sexp ?branching_factor
      (Raw_observer.tuple4 dom1 dom2 dom3 dom4)
      rng ~sexp_of_rng
    >>| fun (f, mk_sexp) ->
    (fun x1 x2 x3 x4 -> f (x1, x2, x3, x4)),
    (fun () -> [%sexp_of: [`curry4 of Sexp.t]] (`curry4 (mk_sexp ())))

  let fn5_with_sexp ?branching_factor dom1 dom2 dom3 dom4 dom5 rng ~sexp_of_rng =
    fn_with_sexp ?branching_factor
      (Raw_observer.tuple5 dom1 dom2 dom3 dom4 dom5)
      rng ~sexp_of_rng
    >>| fun (f, mk_sexp) ->
    (fun x1 x2 x3 x4 x5 -> f (x1, x2, x3, x4, x5)),
    (fun () -> [%sexp_of: [`curry5 of Sexp.t]] (`curry5 (mk_sexp ())))

  let fn6_with_sexp ?branching_factor dom1 dom2 dom3 dom4 dom5 dom6 rng ~sexp_of_rng =
    fn_with_sexp ?branching_factor
      (Raw_observer.tuple6 dom1 dom2 dom3 dom4 dom5 dom6)
      rng ~sexp_of_rng
    >>| fun (f, mk_sexp) ->
    (fun x1 x2 x3 x4 x5 x6 -> f (x1, x2, x3, x4, x5, x6)),
    (fun () -> [%sexp_of: [`curry6 of Sexp.t]] (`curry6 (mk_sexp ())))

  let compare_fn_with_sexp ?branching_factor dom =
    fn_with_sexp ?branching_factor dom For_int.gen
      ~sexp_of_rng:[%sexp_of: int]
    >>| fun (get_index, mk_sexp) ->
    (fun x y -> [%compare: int] (get_index x) (get_index y)),
    (fun () ->
       [%sexp_of: [`compare_using_index_fn of Sexp.t]]
         (`compare_using_index_fn (mk_sexp ())))

  let equal_fn_with_sexp ?branching_factor dom =
    compare_fn_with_sexp ?branching_factor dom
    >>| fun (cmp, mk_sexp) ->
    (fun x y -> Pervasives.( = ) (cmp x y) 0),
    (fun () ->
       [%sexp_of: [`equal_fn_of_compare_fn of Sexp.t]]
         (`equal_fn_of_compare_fn (mk_sexp ())))

  let fn ?branching_factor a b =
    fn_with_sexp  ?branching_factor a b           ~sexp_of_rng:[%sexp_of: _] >>| fst
  let fn2 ?branching_factor a b c =
    fn2_with_sexp ?branching_factor a b c         ~sexp_of_rng:[%sexp_of: _] >>| fst
  let fn3 ?branching_factor a b c d =
    fn3_with_sexp ?branching_factor a b c d       ~sexp_of_rng:[%sexp_of: _] >>| fst
  let fn4 ?branching_factor a b c d e =
    fn4_with_sexp ?branching_factor a b c d e     ~sexp_of_rng:[%sexp_of: _] >>| fst
  let fn5 ?branching_factor a b c d e f =
    fn5_with_sexp ?branching_factor a b c d e f   ~sexp_of_rng:[%sexp_of: _] >>| fst
  let fn6 ?branching_factor a b c d e f g =
    fn6_with_sexp ?branching_factor a b c d e f g ~sexp_of_rng:[%sexp_of: _] >>| fst

  let compare_fn ?branching_factor a = compare_fn_with_sexp ?branching_factor a >>| fst
  let equal_fn   ?branching_factor a = equal_fn_with_sexp   ?branching_factor a >>| fst

end

module Observer = struct

  include Raw_observer

  let of_predicate a b ~f ~f_sexp =
    unmap (variant2 a b)
      ~f:(fun x -> if f x then `A x else `B x)
      ~f_sexp:(fun () ->
        [%sexp_of: [`variant_by_predicate of Sexp.t]]
          (`variant_by_predicate (f_sexp ())))

  let doubleton f ~f_sexp =
    of_predicate (singleton ()) (singleton ()) ~f ~f_sexp

  module Make_int_observer (M : Quickcheck_intf.Pre_int) : sig
    val obs : M.t t
    val obs_between
      :  lower_bound:M.t Maybe_bound.t
      -> upper_bound:M.t Maybe_bound.t
      -> M.t t
  end = struct

    open M

    let bit_is_one index =
      unmap (variant2 (singleton ()) (singleton ()))
        ~f:(fun t -> if bit_and t (shift_left one index) <> zero then `A () else `B ())
        ~f_sexp:(fun () -> [%sexp_of: [`bit_is_one of int]] (`bit_is_one index))

    let non_negative_up_to_inclusive upper_bound =
      let bit_count = count_bits_non_negative (module M) upper_bound in
      (* These weights are chosen to bias observations towards the most and least
         significant bits and away from the "center" bits. *)
      weighted_union
        (List.init bit_count ~f:(fun i ->
           let inverse_wt =
             Pervasives.succ
               (count_bits_non_negative (module Pre_int)
                  (Pervasives.min i (Pervasives.( - ) bit_count i)))
           in
           1. /. Pervasives.float_of_int inverse_wt,
           bit_is_one i))

    let non_negative_between_inclusive ~lower_bound ~upper_bound =
      unmap (non_negative_up_to_inclusive (upper_bound - lower_bound))
        ~f:(fun x -> x - lower_bound)
        ~f_sexp:(fun () -> [%sexp_of: [`decrement_by of t]] (`decrement_by lower_bound))

    let negative_between_inclusive ~lower_bound ~upper_bound =
      unmap (non_negative_between_inclusive
               ~lower_bound:(bit_not upper_bound)
               ~upper_bound:(bit_not lower_bound))
        ~f:bit_not
        ~f_sexp:(fun () -> Sexp.Atom "bit_not")

    let signed_between_inclusive ~lower_bound ~upper_bound =
      unmap (variant2
               (non_negative_between_inclusive ~lower_bound:zero ~upper_bound)
               (negative_between_inclusive ~lower_bound ~upper_bound:(- one)))
        ~f:(fun x -> if x < zero then `A x else `B x)
        ~f_sexp:(fun () -> Sexp.Atom "variant_by_sign")

    let obs_between_inclusive ~lower_bound ~upper_bound =
      if lower_bound >= upper_bound
      then singleton ()
      else if lower_bound >= zero
      then non_negative_between_inclusive ~lower_bound ~upper_bound
      else if upper_bound < zero
      then negative_between_inclusive ~lower_bound ~upper_bound
      else signed_between_inclusive ~lower_bound ~upper_bound

    let obs_between ~lower_bound ~upper_bound =
      match
        (lower_bound : t Maybe_bound.t),
        (upper_bound : t Maybe_bound.t)
      with
      | Excl lower, _ when lower = max_value ->
        failwith "Int.obs_between: lower bound > max_value"
      | _, Excl upper when upper = min_value ->
        failwith "Int.obs_between: upper bound < min_value"
      | _ ->
        let lower_inclusive =
          match lower_bound with
          | Unbounded        -> min_value
          | Incl lower_bound -> lower_bound
          | Excl lower_bound -> lower_bound + one
        in
        let upper_inclusive =
          match upper_bound with
          | Unbounded        -> max_value
          | Incl upper_bound -> upper_bound
          | Excl upper_bound -> upper_bound - one
        in
        if lower_inclusive > upper_inclusive then
          Error.failwiths "Int.obs_between: bounds are crossed"
            (`lower_bound lower_bound, `upper_bound upper_bound)
            [%sexp_of: [`lower_bound of t Maybe_bound.t] *
                       [`upper_bound of t Maybe_bound.t]];
        obs_between_inclusive
          ~lower_bound:lower_inclusive
          ~upper_bound:upper_inclusive

    let obs =
      obs_between
        ~lower_bound:Unbounded
        ~upper_bound:Unbounded

  end

  module For_int = Make_int_observer (Pre_int)

  let enum n ~f ~f_sexp =
    let index =
      For_int.obs_between
        ~lower_bound:(Incl 0)
        ~upper_bound:(Excl n)
    in
    unmap index ~f ~f_sexp

  let of_list list ~equal ~sexp_of_elt =
    let f x =
      match List.findi list ~f:(fun _ y -> equal x y) with
      | None        -> failwith "Quickcheck.Observer.of_list: value not found"
      | Some (i, _) -> i
    in
    enum (List.length list) ~f
      ~f_sexp:(fun () ->
        [%sexp_of: [`find_index_in_list of elt list]]
          (`find_index_in_list list))

  let recursive f =
    let rec self () = f (of_fun self) in
    of_fun self

  let variant3 a b c =
    unmap (variant2 a (variant2 b c)) ~f:(function
      | `A x -> `A x
      | `B x -> `B (`A x)
      | `C x -> `B (`B x))
      ~f_sexp:(fun () -> Sexp.Atom "variant_of_variant3")

  let variant4 a b c d =
    unmap (variant2 (variant2 a b) (variant2 c d)) ~f:(function
      | `A x -> `A (`A x)
      | `B x -> `A (`B x)
      | `C x -> `B (`A x)
      | `D x -> `B (`B x))
      ~f_sexp:(fun () -> Sexp.Atom "variant_of_variant4")

  let variant5 a b c d e =
    unmap (variant2 (variant2 a b) (variant2 c (variant2 d e))) ~f:(function
      | `A x -> `A (`A x)
      | `B x -> `A (`B x)
      | `C x -> `B (`A x)
      | `D x -> `B (`B (`A x))
      | `E x -> `B (`B (`B x)))
      ~f_sexp:(fun () -> Sexp.Atom "variant_of_variant5")

  let variant6 a b c d e f =
    unmap (variant2 (variant2 a (variant2 b c)) (variant2 d (variant2 e f))) ~f:(function
      | `A x -> `A (`A x)
      | `B x -> `A (`B (`A x))
      | `C x -> `A (`B (`B x))
      | `D x -> `B (`A x)
      | `E x -> `B (`B (`A x))
      | `F x -> `B (`B (`B x)))
      ~f_sexp:(fun () -> Sexp.Atom "variant_of_variant6")

  let comparison ~compare ~eq ~lt ~gt ~compare_sexp ~sexp_of_eq =
    unmap (variant3 lt (singleton ()) gt) ~f:(fun x ->
      let c = compare x eq in
      if c < 0 then `A x else
      if c > 0 then `C x else
        `B x)
      ~f_sexp:(fun () ->
        [%sexp_of: [`variant3_by_comparison_to of (eq * Sexp.t)]]
          (`variant3_by_comparison_to (eq, compare_sexp ())))

end

module Shrinker = struct

  open Sexplib.Std
  module Sexp = Sexplib.Sexp
  module List = Core_list0

  type 'a t = ('a -> 'a Sequence.t) Staged.t

  let shrink t = Staged.unstage t

  let create t = Staged.stage t

  let empty () = create (fun _ -> Sequence.empty)

  let map t ~f ~f_inverse =
    create (fun b ->
      shrink t (f_inverse b)
      |> Sequence.map ~f)

  module Test_data = struct
    let singleton equal min =
      create (fun v ->
        if equal min v
        then Sequence.empty
        else Sequence.singleton min)

    let%test_module "singleton" =
      (module struct
        let t = singleton Pervasives.(=) 42

        let%test_unit "singleton produces values" =
          let shrunk = shrink t 2 |> Sequence.to_list in
          let expect = [42] in
          [%test_result: int list ] ~expect shrunk

        let%test_unit "singleton doesn't produce the input" =
          let shrunk = shrink t 42 |> Sequence.to_list in
          let expect = [] in
          [%test_result: int list ] ~expect shrunk
      end)


    let t0 = singleton Pervasives.(=) 0
    let t1 = singleton Pervasives.(=) 1
    let t2 = singleton Pervasives.(=) 2
    let t3 = singleton Pervasives.(=) 3
    let t4 = singleton Pervasives.(=) 4
    let t5 = singleton Pervasives.(=) 5
  end

  let tuple2 t1 t2 =
    let shrinker (v1, v2) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x))  in
      Sequence.interleave (Sequence.of_list [v1_seq; v2_seq])
    in
    create shrinker

  let tuple3 t1 t2 t3 =
    let shrinker (v1, v2, v3) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x))  in
      Sequence.interleave (Sequence.of_list [v1_seq; v2_seq; v3_seq])
    in
    create shrinker

  let tuple4 t1 t2 t3 t4 =
    let shrinker (v1, v2, v3, v4) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3, v4)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3, v4)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x,  v4)) in
      let v4_seq = Sequence.map (shrink t4 v4) ~f:(fun x -> (v1, v2, v3, x))  in
      Sequence.interleave (Sequence.of_list [v1_seq; v2_seq; v3_seq; v4_seq])
    in
    create shrinker

  let tuple5 t1 t2 t3 t4 t5 =
    let shrinker (v1, v2, v3, v4, v5) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3, v4, v5)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3, v4, v5)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x,  v4, v5)) in
      let v4_seq = Sequence.map (shrink t4 v4) ~f:(fun x -> (v1, v2, v3, x,  v5)) in
      let v5_seq = Sequence.map (shrink t5 v5) ~f:(fun x -> (v1, v2, v3, v4, x))  in
      Sequence.interleave (Sequence.of_list [v1_seq; v2_seq; v3_seq; v4_seq; v5_seq])
    in
    create shrinker

  let tuple6 t1 t2 t3 t4 t5 t6 =
    let shrinker (v1, v2, v3, v4, v5, v6) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3, v4, v5, v6)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3, v4, v5, v6)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x,  v4, v5, v6)) in
      let v4_seq = Sequence.map (shrink t4 v4) ~f:(fun x -> (v1, v2, v3, x,  v5, v6)) in
      let v5_seq = Sequence.map (shrink t5 v5) ~f:(fun x -> (v1, v2, v3, v4, x,  v6)) in
      let v6_seq = Sequence.map (shrink t6 v6) ~f:(fun x -> (v1, v2, v3, v4, v5, x))  in
      Sequence.interleave (Sequence.of_list [v1_seq; v2_seq; v3_seq; v4_seq; v5_seq; v6_seq])
    in
    create shrinker

  let%test_module "tuple shrinkers" =
    (module struct
      open Test_data

      let%test_unit "tuple2 shrinker" =
        let sort = List.sort ~cmp:[%compare: int * int ] in
        let expect =
          [(0,5); (5,1)]
          |> sort
        in
        let results =
          shrink (tuple2 t0 t1) (5,5)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int * int) list ] ~expect results

      let%test_unit "tuple3 shrinker" =
        let sort = List.sort ~cmp:[%compare: int * int * int ] in
        let expect = [(0,5,5); (5,1,5); (5,5,2)] |> sort in
        let results =
          shrink (tuple3 t0 t1 t2) (5,5,5)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int*int*int) list ] results ~expect

      let%test_unit "tuple4 shrinker" =
        let sort = List.sort ~cmp:[%compare: int * int * int * int ] in
        let expect =
          [(0,5,5,5); (5,1,5,5); (5,5,2,5); (5,5,5,3)]
          |> sort
        in
        let results =
          shrink (tuple4 t0 t1 t2 t3) (5,5,5,5)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int*int*int*int) list ] results ~expect

      let%test_unit "tuple5 shrinker" =
        let sort = List.sort ~cmp:[%compare: int * int * int * int * int ] in
        let expect =
          [(0,5,5,5,5); (5,1,5,5,5); (5,5,2,5,5); (5,5,5,3,5); (5,5,5,5,4)]
          |> sort
        in
        let results =
          shrink (tuple5 t0 t1 t2 t3 t4) (5,5,5,5,5)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int*int*int*int*int) list ] results ~expect

      let%test_unit "tuple6 shrinker" =
        let sort = List.sort ~cmp:[%compare: int * int * int * int * int * int ] in
        let expect =
          [ (0,9,9,9,9,9)
          ; (9,1,9,9,9,9)
          ; (9,9,2,9,9,9)
          ; (9,9,9,3,9,9)
          ; (9,9,9,9,4,9)
          ; (9,9,9,9,9,5)
          ]
          |> sort
        in
        let results =
          shrink (tuple6 t0 t1 t2 t3 t4 t5) (9,9,9,9,9,9)
          |> Sequence.to_list |> sort
        in
        [%test_result: (int*int*int*int*int*int) list ] results ~expect

    end)

  let variant2 t_a t_b : [ `A of 'a | `B of 'b ] t =
    let shrinker var =
      match var with
      | `A a -> Sequence.map (shrink t_a a) ~f:(fun a -> (`A a))
      | `B b -> Sequence.map (shrink t_b b) ~f:(fun b -> (`B b))
    in
    create shrinker

  let variant3 t_a t_b t_c =
    let shrinker var =
      match var with
      | `A v -> Sequence.map (shrink t_a v) ~f:(fun v -> (`A v))
      | `B v -> Sequence.map (shrink t_b v) ~f:(fun v -> (`B v))
      | `C v -> Sequence.map (shrink t_c v) ~f:(fun v -> (`C v))
    in
    create shrinker

  let variant4 t_a t_b t_c t_d =
    let shrinker var =
      match var with
      | `A v -> Sequence.map (shrink t_a v) ~f:(fun v -> (`A v))
      | `B v -> Sequence.map (shrink t_b v) ~f:(fun v -> (`B v))
      | `C v -> Sequence.map (shrink t_c v) ~f:(fun v -> (`C v))
      | `D v -> Sequence.map (shrink t_d v) ~f:(fun v -> (`D v))
    in
    create shrinker

  let variant5 t_a t_b t_c t_d t_e =
    let shrinker var =
      match var with
      | `A v -> Sequence.map (shrink t_a v) ~f:(fun v -> (`A v))
      | `B v -> Sequence.map (shrink t_b v) ~f:(fun v -> (`B v))
      | `C v -> Sequence.map (shrink t_c v) ~f:(fun v -> (`C v))
      | `D v -> Sequence.map (shrink t_d v) ~f:(fun v -> (`D v))
      | `E v -> Sequence.map (shrink t_e v) ~f:(fun v -> (`E v))
    in
    create shrinker

  let variant6 t_a t_b t_c t_d t_e t_f =
    let shrinker var =
      match var with
      | `A v -> Sequence.map (shrink t_a v) ~f:(fun v -> (`A v))
      | `B v -> Sequence.map (shrink t_b v) ~f:(fun v -> (`B v))
      | `C v -> Sequence.map (shrink t_c v) ~f:(fun v -> (`C v))
      | `D v -> Sequence.map (shrink t_d v) ~f:(fun v -> (`D v))
      | `E v -> Sequence.map (shrink t_e v) ~f:(fun v -> (`E v))
      | `F v -> Sequence.map (shrink t_f v) ~f:(fun v -> (`F v))
    in
    create shrinker

  let%test_module "variant shrinkers" =
    (module struct
      open Test_data

      type var2 = [ `A of int | `B of int ] [@@deriving sexp, compare]
      type var3 = [ `A of int | `B of int | `C of int ] [@@deriving sexp, compare]
      type var4 = [ `A of int | `B of int | `C of int | `D of int ] [@@deriving sexp, compare]
      type var5 = [ `A of int | `B of int | `C of int | `D of int | `E of int ]
        [@@deriving sexp, compare]
      type var6 = [ `A of int | `B of int | `C of int | `D of int | `E of int | `F of int ]
        [@@deriving sexp, compare]

      let%test_unit "variant2 shrinker" =
        let t = variant2 t0 t1 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var2 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var2 list ] ~expect:[`B 1] shrunk_b

      let%test_unit "variant3 shrinker" =
        let t = variant3 t0 t1 t2 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var3 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var3 list ] ~expect:[`B 1] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var3 list ] ~expect:[`C 2] shrunk_c

      let%test_unit "variant4 shrinker" =
        let t = variant4 t0 t1 t2 t3 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var4 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var4 list ] ~expect:[`B 1] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var4 list ] ~expect:[`C 2] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var4 list ] ~expect:[`D 3] shrunk_d

      let%test_unit "variant5 shrinker" =
        let t = variant5 t0 t1 t2 t3 t4 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`B 1] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`C 2] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`D 3] shrunk_d;
        let shrunk_e = shrink t (`E 1) |> Sequence.to_list in
        [%test_result: var5 list ] ~expect:[`E 4] shrunk_e

      let%test_unit "variant6 shrinker" =
        let t = variant6 t0 t1 t2 t3 t4 t5 in
        let shrunk_a = shrink t (`A 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`A 0] shrunk_a;
        let shrunk_b = shrink t (`B 0) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`B 1] shrunk_b;
        let shrunk_c = shrink t (`C 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`C 2] shrunk_c;
        let shrunk_d = shrink t (`D 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`D 3] shrunk_d;
        let shrunk_e = shrink t (`E 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`E 4] shrunk_e;
        let shrunk_f = shrink t (`F 1) |> Sequence.to_list in
        [%test_result: var6 list ] ~expect:[`F 5] shrunk_f
    end)

  let lazy_sequence thunk =
    Sequence.unfold_step ~init:(`start thunk)
      ~f:(function
        | `start thunk -> Skip (`cont (thunk ()))
        | `cont  seq   ->
          match Sequence.next seq with
          | None         -> Done
          | Some (v, tl) -> Yield (v, `cont tl))

  let recursive f =
    let rec shrinker v =
      lazy_sequence (fun () -> shrink (f (create shrinker)) v)
    in
    create shrinker

  module Make_int_shrinker (M : Quickcheck_intf.Pre_int) : sig
    val shrinker  : M.t t
  end = struct
    (* Having smaller numbers generally doesn't make a bug easier to
       understand, it is usually much more useful to let other shrinkers have
       more attempts to reduce the size of the datastructure. *)
    let shrinker = empty ()
  end

  module For_int = Make_int_shrinker (Pre_int)

end

module Make_int (M : Quickcheck_intf.Pre_int)
  : Quickcheck_intf.S_bounded
    with type    t   :=    M.t
    with type 'a gen := 'a gen
    with type 'a obs := 'a obs
    with type 'a shr := 'a Shrinker.t
= struct

  include Shrinker.Make_int_shrinker   (M)
  include Generator.Make_int_generator (M)
  include Observer.Make_int_observer   (M)

end

module For_int = struct
  include Shrinker.For_int
  include Generator.For_int
  include Observer.For_int
end

module Configure (Config : Quickcheck_intf.Quickcheck_config) = struct

  include Config

  type 'a shr = 'a Shrinker.t

  let scale ~n ~f = Pervasives.int_of_float (f *. Pervasives.float_of_int n)

  let computed_default_trial_count_for_test_no_duplicates =
    match default_trial_count_for_test_no_duplicates with
    | `Constant                     n -> n
    | `Scale_of_default_trial_count f -> scale ~f ~n:default_trial_count

  let random_state_of_seed seed =
    match seed with
    | `Nondeterministic  -> Core_random.State.make_self_init ()
    | `Deterministic str ->
      let array = Array.init (String.length str) ~f:(fun i -> Char.code str.[i]) in
      Core_random.State.make array

  let random_sequence
        ?(seed = default_seed)
        ?(probability_threshold_to_remember_choice
          = default_probability_threshold_to_remember_choice)
        gen =
    let random_state = random_state_of_seed seed in
    let random_float_between_zero_and_one () =
      Core_random.State.float random_state 1.
    in
    Sequence.unfold_step ~init:gen ~f:(fun gen ->
      match
        Generator.choose gen
          ~random_float_between_zero_and_one
          ~max_attempts:Pervasives.max_int
      with
      | `Ran_out_of_attempts -> assert false
      | `No_choices_remain   -> Done
      | `Choice choice       ->
        let gen' =
          if Pre_float.( >= )
               (Generator.Choice.probability choice)
               probability_threshold_to_remember_choice
          then Generator.Choice.updated_gen choice ~keep:`All_choices_except_this_choice
          else Generator.Choice.original_gen choice
        in
        let value =
          Generator.Choice.value choice
        in
        Yield (value, gen'))

  let iter
        ?(seed     = default_seed)
        ?(trials   = default_trial_count)
        ?(attempts = scale ~n:trials ~f:default_attempts_per_trial)
        ?(probability_threshold_to_remember_choice
          = default_probability_threshold_to_remember_choice)
        gen ~f =
    let random_state = random_state_of_seed seed in
    let random_float_between_zero_and_one () =
      Core_random.State.float random_state 1.
    in
    let rec loop gen ~remaining_trials ~remaining_attempts =
      if remaining_trials < 1 then () else
        match
          Generator.choose gen
            ~max_attempts:remaining_attempts
            ~random_float_between_zero_and_one
        with
        | `No_choices_remain   -> ()
        | `Ran_out_of_attempts ->
          failwith
            (Printf.sprintf "Quickcheck.iter: failed to generate %d inputs in %d attempts"
               trials
               attempts)
        | `Choice choice ->
          f (Generator.Choice.value choice);
          let gen' =
            if Pre_float.( >= )
                 (Generator.Choice.probability choice)
                 probability_threshold_to_remember_choice
            then Generator.Choice.updated_gen choice ~keep:`All_choices_except_this_choice
            else Generator.Choice.original_gen choice
          in
          let attempts_used =
            Generator.Choice.attempts_used choice
          in
          loop gen'
            ~remaining_trials:   (remaining_trials   - 1)
            ~remaining_attempts: (remaining_attempts - attempts_used)
    in
    loop gen ~remaining_trials:trials ~remaining_attempts:attempts

  let random_value ?seed gen =
    let r = ref None in
    iter ?seed ~trials:1 gen ~f:(fun x -> r := Some x);
    match !r with
    | None   -> raise Not_found
    | Some x -> x

  exception Random_input of Sexp.t * exn [@@deriving sexp]
  exception Shrunk_random_input of
      [ `Original of Sexp.t * exn ] * [ `Shrunk of Sexp.t * exn ]
  [@@deriving sexp]

  let shrink_iter
        ?sexp_of
        ~value
        ~exn
        ~shrinker
        ?(shrink_attempts = default_shrink_attempts)
        ~f =
    let within_bounds attempts=
      match shrink_attempts with
      | `Exhaustive -> true
      | `Limit n    -> attempts < n
    in
    let rec shrink_loop seq attempts result =
      if within_bounds attempts then
        match Sequence.next seq with
        | None                     -> result
        | Some (shr_value, seq_tl) ->
          match f shr_value with
          | ()                -> shrink_loop seq_tl (attempts+1) result
          | exception shr_exn ->
            let seq = Shrinker.shrink shrinker shr_value in
            shrink_loop seq (attempts+1) (Some (shr_value, shr_exn))
      else
        result
    in
    match shrink_loop (Shrinker.shrink shrinker value) 0 None with
    | Some (shr_value, shr_exn) ->
      let sexp_of =
        match sexp_of with
        | Some f -> f
        | None   -> [%sexp_of: _]
      in
      raise (Shrunk_random_input
               (`Original (sexp_of value,     exn),
                `Shrunk   (sexp_of shr_value, shr_exn)))
    | None ->
      match sexp_of with
      | None             -> raise exn
      | Some sexp_of_arg -> raise (Random_input (sexp_of_arg value, exn))

  let test
        ?seed
        ?trials
        ?attempts
        ?shrinker
        ?shrink_attempts
        ?probability_threshold_to_remember_choice
        ?sexp_of
        ?(examples = [])
        gen
        ~f
    =
    let f' =
      match shrinker with
      | Some shrinker ->
        (fun x ->
           try f x with exn ->
             shrink_iter ~value:x ~exn ?sexp_of ~shrinker ?shrink_attempts ~f)
      | None ->
        match sexp_of with
        | Some sexp_of_arg ->
          (fun x ->
             try f x with exn ->
               raise (Random_input (sexp_of_arg x, exn)))
        | None -> f
    in
    List.iter examples ~f:f';
    iter ?seed ?trials ?attempts ?probability_threshold_to_remember_choice gen ~f:f'

  exception Duplicate_value [@@deriving sexp]
  exception Duplicate_values of Sexp.t * Sexp.t [@@deriving sexp]

  let test_no_duplicates (type a)
        ?seed
        ?(trials = computed_default_trial_count_for_test_no_duplicates)
        ?attempts
        ?probability_threshold_to_remember_choice
        ?sexp_of
        gen ~by =
    let fail =
      match sexp_of with
      | None   -> fun _  _  -> raise Duplicate_value
      | Some f -> fun v1 v2 -> raise (Duplicate_values (f v1, f v2))
    in
    let f =
      match by with
      | `Equal equal ->
        let r = ref [] in
        (fun x ->
           match List.find !r ~f:(fun y -> equal x y) with
           | Some y -> fail x y
           | None   -> r := x :: !r)
      | `Compare compare ->
        let module T = struct
          type t = a
          let compare = compare
        end in
        let module M = Caml.Map.Make (T) in
        let map = ref M.empty in
        (fun x ->
           match M.find x !map with
           | y                   -> fail x y
           | exception Not_found -> map := M.add x x !map)
    in
    test ?seed ~trials ?attempts ?probability_threshold_to_remember_choice ?sexp_of gen ~f

  exception Can_generate
  exception Cannot_generate [@@deriving sexp]
  exception Cannot_generate_from_any_of of Sexp.t [@@deriving sexp]

  let test_can_generate
        ?seed
        ?trials
        ?attempts
        ?probability_threshold_to_remember_choice
        ?sexp_of
        gen ~f =
    let r = ref [] in
    let f_and_enqueue x = r := x :: !r; if f x then raise Can_generate in
    match
      iter
        ?seed
        ?trials
        ?attempts
        ?probability_threshold_to_remember_choice
        gen
        ~f:f_and_enqueue
    with
    | exception Can_generate -> ()
    | () ->
      match sexp_of with
      | None ->
        raise Cannot_generate
      | Some sexp_of_value ->
        raise (Cannot_generate_from_any_of ([%sexp_of: value list] !r))

end

include Configure (struct
    let default_seed = `Deterministic "an arbitrary but deterministic string"
    let default_trial_count                              = 100
    let default_attempts_per_trial                       = 10.
    let default_shrink_attempts                          = `Limit 1000
    let default_probability_threshold_to_remember_choice = epsilon_float
    let default_trial_count_for_test_no_duplicates
      = `Scale_of_default_trial_count 10.
  end)

let%bench_module "Quickcheck.iter" =
  (module struct

    let rec list_gen gen ~len =
      if len = 0 then Generator.singleton [] else
        begin
          let open Generator.Monad_infix in
          gen                       >>= fun elem ->
          list_gen gen ~len:(len-1) >>| fun rest ->
          elem :: rest
        end

    let bool_gen = Generator.of_list [ true ; false ]

    let gen = list_gen bool_gen ~len:100

    let make_bench_fun probability_threshold_to_remember_choice =
      (fun () -> iter gen ~f:ignore ~probability_threshold_to_remember_choice)

    let%bench_fun "remember" = make_bench_fun 0.
    let%bench_fun "forget"   = make_bench_fun epsilon_float

  end)
