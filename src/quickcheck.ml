open Sexplib.Std

module Sexp  = Sexplib.Sexp
module List  = ListLabels
module Array = ArrayLabels

open Quickcheck_intf

module Configure (Config : Quickcheck_config) = struct

  module Generator = Quickcheck_generator
  module Observer  = Quickcheck_observer

  include Config

  let scale ~n ~f = int_of_float (f *. float_of_int n)

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

  let random_sequence ?(seed = default_seed) gen =
    let random_state = random_state_of_seed seed in
    let random_float_between_zero_and_one () =
      Core_random.State.float random_state 1.
    in
    Sequence.unfold_step ~init:gen ~f:(fun gen ->
      match
        Generator.choose gen
          ~random_float_between_zero_and_one
          ~max_attempts:Core_int.max_value
      with
      | `Ran_out_of_attempts -> assert false
      | `No_choices_remain   -> Done
      | `Choice choice       ->
        let gen' =
          Generator.Choice.updated_gen choice
            ~keep:`All_choices_except_this_choice
        in
        let value =
          Generator.Choice.value choice
        in
        Yield (value, gen'))

  let iter
        ?(seed     = default_seed)
        ?(trials   = default_trial_count)
        ?(attempts = scale ~n:trials ~f:default_attempts_per_trial)
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
            Generator.Choice.updated_gen choice
              ~keep:`All_choices_except_this_choice
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

  exception Random_input of Sexp.t * exn with sexp

  let test ?seed ?trials ?attempts ?sexp_of ?(examples = []) gen ~f =
    let f_with_sexp =
      match sexp_of with
      | None -> f
      | Some sexp_of_arg ->
        (fun x ->
           try f x with exn ->
             raise (Random_input (sexp_of_arg x, exn)))
    in
    List.iter examples ~f:f_with_sexp;
    iter ?seed ?trials ?attempts gen ~f:f_with_sexp

  exception Duplicate_value with sexp
  exception Duplicate_values of Sexp.t * Sexp.t with sexp

  let test_no_duplicates (type a)
        ?seed
        ?(trials = computed_default_trial_count_for_test_no_duplicates)
        ?attempts
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
           | y                   -> fail x y
           | exception Not_found -> r := x :: !r)
      | `Compare compare ->
        let module T = struct
          type t = a
          let compare = compare
        end in
        let module M = Map.Make (T) in
        let map = ref M.empty in
        (fun x ->
           match M.find x !map with
           | y                   -> fail x y
           | exception Not_found -> map := M.add x x !map)
    in
    test ?seed ~trials ?attempts ?sexp_of gen ~f

  exception Can_generate
  exception Cannot_generate with sexp
  exception Cannot_generate_from_any_of of Sexp.t with sexp

  let test_can_generate ?seed ?trials ?attempts ?sexp_of gen ~f =
    let r = ref [] in
    let f_and_enqueue x = r := x :: !r; if f x then raise Can_generate in
    match iter ?seed ?trials ?attempts gen ~f:f_and_enqueue with
    | exception Can_generate -> ()
    | () ->
      match sexp_of with
      | None ->
        raise Cannot_generate
      | Some sexp_of_value ->
        raise (Cannot_generate_from_any_of (<:sexp_of< value list >> !r))

end

include Configure (struct
    let default_seed = `Deterministic "an arbitrary but deterministic string"
    let default_trial_count        = 100
    let default_attempts_per_trial = 10.
    let default_trial_count_for_test_no_duplicates =
      `Scale_of_default_trial_count 10.
  end)
