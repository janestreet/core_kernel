open! Import

open Quickcheck_intf
open Base_quickcheck

module Array      = Base.Array
module Bool       = Base.Bool
module Char       = Base.Char
module Int        = Base.Int
module List       = Base.List
module Option     = Base.Option
module Type_equal = Base.Type_equal

module Pre_float : Comparisons.S with type t = float = struct
  type t = float

  open Pervasives

  let compare (x : t) (y : t) = compare x y
  let equal   (x : t) (y : t) = compare x y = 0
  let min     (x : t) (y : t) = min     x y
  let max     (x : t) (y : t) = max     x y

  let ( >= ) (x : t) (y : t) = (x >= y)
  let ( <= ) (x : t) (y : t) = (x <= y)
  let ( =  ) (x : t) (y : t) = (x =  y)
  let ( >  ) (x : t) (y : t) = (x >  y)
  let ( <  ) (x : t) (y : t) = (x <  y)
  let ( <> ) (x : t) (y : t) = (x <> y)
end

module Pre_int : Pre_int with type t = int = struct
  include Base.Int
  let splittable_random             = Splittable_random.int
  let splittable_random_log_uniform = Splittable_random.Log_uniform.int
end

module Observer = struct
  include Observer

  let of_hash (type a) (module M : Deriving_hash with type t = a) =
    of_hash_fold M.hash_fold_t

  let variant2 t1 t2 =
    create (fun x ~size ~hash ->
      match x with
      | `A y -> observe t1 y ~size ~hash:([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ~hash:([%hash_fold: int] hash 2))

  let variant3 t1 t2 t3 =
    create (fun x ~size ~hash ->
      match x with
      | `A y -> observe t1 y ~size ~hash:([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ~hash:([%hash_fold: int] hash 2)
      | `C y -> observe t3 y ~size ~hash:([%hash_fold: int] hash 3))

  let variant4 t1 t2 t3 t4 =
    create (fun x ~size ~hash ->
      match x with
      | `A y -> observe t1 y ~size ~hash:([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ~hash:([%hash_fold: int] hash 2)
      | `C y -> observe t3 y ~size ~hash:([%hash_fold: int] hash 3)
      | `D y -> observe t4 y ~size ~hash:([%hash_fold: int] hash 4))

  let variant5 t1 t2 t3 t4 t5 =
    create (fun x ~size ~hash ->
      match x with
      | `A y -> observe t1 y ~size ~hash:([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ~hash:([%hash_fold: int] hash 2)
      | `C y -> observe t3 y ~size ~hash:([%hash_fold: int] hash 3)
      | `D y -> observe t4 y ~size ~hash:([%hash_fold: int] hash 4)
      | `E y -> observe t5 y ~size ~hash:([%hash_fold: int] hash 5))

  let variant6 t1 t2 t3 t4 t5 t6 =
    create (fun x ~size ~hash ->
      match x with
      | `A y -> observe t1 y ~size ~hash:([%hash_fold: int] hash 1)
      | `B y -> observe t2 y ~size ~hash:([%hash_fold: int] hash 2)
      | `C y -> observe t3 y ~size ~hash:([%hash_fold: int] hash 3)
      | `D y -> observe t4 y ~size ~hash:([%hash_fold: int] hash 4)
      | `E y -> observe t5 y ~size ~hash:([%hash_fold: int] hash 5)
      | `F y -> observe t6 y ~size ~hash:([%hash_fold: int] hash 6))

  let (|>!) hash f = f ~hash

  let tuple2 t1 t2 =
    create (fun (x1,x2) ~size ~hash ->
      hash
      |>! observe t1 x1 ~size
      |>! observe t2 x2 ~size)

  let tuple3 t1 t2 t3 =
    create (fun (x1,x2,x3) ~size ~hash ->
      hash
      |>! observe t1 x1 ~size
      |>! observe t2 x2 ~size
      |>! observe t3 x3 ~size)

  let tuple4 t1 t2 t3 t4 =
    create (fun (x1,x2,x3,x4) ~size ~hash ->
      hash
      |>! observe t1 x1 ~size
      |>! observe t2 x2 ~size
      |>! observe t3 x3 ~size
      |>! observe t4 x4 ~size)

  let tuple5 t1 t2 t3 t4 t5 =
    create (fun (x1,x2,x3,x4,x5) ~size ~hash ->
      hash
      |>! observe t1 x1 ~size
      |>! observe t2 x2 ~size
      |>! observe t3 x3 ~size
      |>! observe t4 x4 ~size
      |>! observe t5 x5 ~size)

  let tuple6 t1 t2 t3 t4 t5 t6 =
    create (fun (x1,x2,x3,x4,x5,x6) ~size ~hash ->
      hash
      |>! observe t1 x1 ~size
      |>! observe t2 x2 ~size
      |>! observe t3 x3 ~size
      |>! observe t4 x4 ~size
      |>! observe t5 x5 ~size
      |>! observe t6 x6 ~size)

  let of_predicate a b ~f =
    unmap (variant2 a b)
      ~f:(fun x -> if f x then `A x else `B x)

  let singleton () = opaque

  let doubleton f =
    of_predicate (singleton ()) (singleton ()) ~f

  let enum _ ~f =
    unmap int ~f

  let of_list list ~equal =
    let f x =
      match List.findi list ~f:(fun _ y -> equal x y) with
      | None        -> failwith "Quickcheck.Observer.of_list: value not found"
      | Some (i, _) -> i
    in
    enum (List.length list) ~f

  let of_fun f =
    create (fun x ~size ~hash ->
      observe (f ()) x ~size ~hash)

  let recursive = fixed_point

  let comparison ~compare ~eq ~lt ~gt =
    unmap (variant3 lt (singleton ()) gt) ~f:(fun x ->
      let c = compare x eq in
      if c < 0 then `A x else
      if c > 0 then `C x else
        `B x)
end

module Generator = struct
  include Generator

  open Let_syntax

  let singleton = return

  let doubleton x y =
    create (fun ~size:_ ~random ->
      if Splittable_random.bool random
      then x
      else y)

  let of_fun f =
    create (fun ~size ~random ->
      generate (f ()) ~size ~random)

  let of_sequence ~p seq =
    if Pervasives.( <= ) p 0. || Pervasives.( > ) p 1. then
      failwith (Printf.sprintf "Generator.of_sequence: probability [%f] out of bounds" p);
    Sequence.delayed_fold seq
      ~init:()
      ~finish:(fun () -> failwith "Generator.of_sequence: ran out of values")
      ~f:(fun () x ~k ->
        weighted_union
          [       p, singleton x
          ; 1. -. p, of_fun k
          ])

  let rec bounded_geometric ~p ~maximum init =
    if init = maximum
    then singleton maximum
    else
      weighted_union
        [       p, singleton init
        ; 1. -. p, of_fun (fun () -> bounded_geometric ~p ~maximum (init + 1))
        ]

  let geometric ~p init =
    bounded_geometric ~p ~maximum:Pre_int.max_value init

  let small_non_negative_int = small_positive_or_zero_int

  let small_positive_int = small_strictly_positive_int

  let recursive = fixed_point

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

  let list_with_length length t = list_with_length t ~length

  let fn2 dom1 dom2 rng =
    fn (Observer.tuple2 dom1 dom2) rng
    >>| fun f ->
    (fun x1 x2 -> f (x1, x2))

  let fn3 dom1 dom2 dom3 rng =
    fn (Observer.tuple3 dom1 dom2 dom3) rng
    >>| fun f ->
    (fun x1 x2 x3 -> f (x1, x2, x3))

  let fn4 dom1 dom2 dom3 dom4 rng =
    fn (Observer.tuple4 dom1 dom2 dom3 dom4) rng
    >>| fun f ->
    (fun x1 x2 x3 x4 -> f (x1, x2, x3, x4))

  let fn5 dom1 dom2 dom3 dom4 dom5 rng =
    fn (Observer.tuple5 dom1 dom2 dom3 dom4 dom5) rng
    >>| fun f ->
    (fun x1 x2 x3 x4 x5 -> f (x1, x2, x3, x4, x5))

  let fn6 dom1 dom2 dom3 dom4 dom5 dom6 rng =
    fn (Observer.tuple6 dom1 dom2 dom3 dom4 dom5 dom6) rng
    >>| fun f ->
    (fun x1 x2 x3 x4 x5 x6 -> f (x1, x2, x3, x4, x5, x6))

  let compare_fn dom =
    fn dom int
    >>| fun get_index ->
    (fun x y -> [%compare: int] (get_index x) (get_index y))

  let equal_fn dom =
    compare_fn dom
    >>| fun cmp ->
    (fun x y -> Pervasives.( = ) (cmp x y) 0)
end

module Shrinker = struct
  include Shrinker

  let empty () = atomic

  let tuple2 t1 t2 =
    let shrinker (v1, v2) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x))  in
      Sequence.round_robin [v1_seq; v2_seq]
    in
    create shrinker

  let tuple3 t1 t2 t3 =
    let shrinker (v1, v2, v3) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x))  in
      Sequence.round_robin [v1_seq; v2_seq; v3_seq]
    in
    create shrinker

  let tuple4 t1 t2 t3 t4 =
    let shrinker (v1, v2, v3, v4) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3, v4)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3, v4)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x,  v4)) in
      let v4_seq = Sequence.map (shrink t4 v4) ~f:(fun x -> (v1, v2, v3, x))  in
      Sequence.round_robin [v1_seq; v2_seq; v3_seq; v4_seq]
    in
    create shrinker

  let tuple5 t1 t2 t3 t4 t5 =
    let shrinker (v1, v2, v3, v4, v5) =
      let v1_seq = Sequence.map (shrink t1 v1) ~f:(fun x -> (x,  v2, v3, v4, v5)) in
      let v2_seq = Sequence.map (shrink t2 v2) ~f:(fun x -> (v1, x,  v3, v4, v5)) in
      let v3_seq = Sequence.map (shrink t3 v3) ~f:(fun x -> (v1, v2, x,  v4, v5)) in
      let v4_seq = Sequence.map (shrink t4 v4) ~f:(fun x -> (v1, v2, v3, x,  v5)) in
      let v5_seq = Sequence.map (shrink t5 v5) ~f:(fun x -> (v1, v2, v3, v4, x))  in
      Sequence.round_robin [v1_seq; v2_seq; v3_seq; v4_seq; v5_seq]
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
      Sequence.round_robin [v1_seq; v2_seq; v3_seq; v4_seq; v5_seq; v6_seq]
    in
    create shrinker

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

  let recursive = fixed_point
end

module Let_syntax = struct
  module Let_syntax = struct
    include Generator
    module Open_on_rhs = Generator
  end
  include Generator.Monad_infix
  let return = Generator.return
end

module Configure (Config : Quickcheck_config) = struct

  include Config

  let nondeterministic_state = lazy (Random.State.make_self_init ())

  let random_state_of_seed seed =
    match seed with
    | `Nondeterministic  -> Splittable_random.State.create (force nondeterministic_state)
    | `Deterministic str -> Splittable_random.State.of_int ([%hash: string] str)

  let make_seed seed : Test.Config.Seed.t =
    match seed with
    | `Nondeterministic     -> Nondeterministic
    | `Deterministic string -> Deterministic string

  let make_shrink_count = function
    | `Exhaustive -> Int.max_value
    | `Limit n -> n

  let make_config ~seed ~sizes ~trials ~shrink_attempts : Test.Config.t =
    {
      seed = make_seed (Option.value seed ~default:default_seed);
      sizes = Option.value sizes ~default:default_sizes;
      test_count = Option.value trials ~default:default_trial_count;
      shrink_count =
        make_shrink_count
          (Option.value shrink_attempts ~default:default_shrink_attempts);
    }

  let make_test_m (type a) ~gen ~shrinker ~sexp_of : (module Test.S with type t = a) =
    (module struct
      type t = a
      let quickcheck_generator = gen
      let quickcheck_shrinker = Option.value shrinker ~default:Shrinker.atomic
      let sexp_of_t = Option.value sexp_of ~default:[%sexp_of: _]
    end)

  let random_value ?(seed = default_seed) ?(size = 30) gen =
    let random = random_state_of_seed seed in
    Generator.generate gen ~size ~random

  let random_sequence ?seed ?sizes gen =
    let config =
      make_config ~seed ~sizes ~trials:(Some Int.max_value) ~shrink_attempts:None
    in
    let return = ref Sequence.empty in
    Test.with_sample_exn ~config gen ~f:(fun sequence ->
      return := sequence);
    !return

  let iter ?seed ?sizes ?trials gen ~f =
    let config = make_config ~seed ~sizes ~trials ~shrink_attempts:None in
    Test.with_sample_exn ~config gen ~f:(fun sequence ->
      Sequence.iter sequence ~f)

  let test ?seed ?sizes ?trials ?shrinker ?shrink_attempts ?sexp_of ?examples gen ~f =
    let config = make_config ~seed ~sizes ~trials ~shrink_attempts in
    let test_m = make_test_m ~gen ~shrinker ~sexp_of in
    Test.run_exn ~config ?examples ~f test_m

  let test_or_error
        ?seed ?sizes ?trials ?shrinker ?shrink_attempts ?sexp_of ?examples gen ~f
    =
    let config = make_config ~seed ~sizes ~trials ~shrink_attempts in
    let test_m = make_test_m ~gen ~shrinker ~sexp_of in
    Test.run ~config ?examples ~f test_m

  let test_distinct_values
        (type key)
        ?seed
        ?sizes
        ?sexp_of
        gen
        ~trials
        ~distinct_values
        ~compare
    =
    let module S = Caml.Set.Make (struct type t = key let compare = compare end) in
    let fail set =
      let expect_count = distinct_values in
      let actual_count = S.cardinal set in
      let values =
        match sexp_of with
        | None             -> None
        | Some sexp_of_elt -> Some [%sexp (S.elements set : elt list)]
      in
      raise_s [%message
        "insufficient distinct values"
          (trials       : int)
          (expect_count : int)
          (actual_count : int)
          (values       : Base.Sexp.t sexp_option)]
    in
    with_return (fun r ->
      let set = ref S.empty in
      iter ?seed ?sizes ~trials gen ~f:(fun elt ->
        set := S.add elt !set;
        if S.cardinal !set >= distinct_values then r.return ());
      fail !set)

  let test_can_generate
        ?seed
        ?sizes
        ?(trials = default_can_generate_trial_count)
        ?sexp_of
        gen ~f =
    let r = ref [] in
    let f_and_enqueue return x =
      if f x
      then return `Can_generate
      else r := x :: !r
    in
    match
      With_return.with_return (fun return ->
        iter
          ?seed
          ?sizes
          ~trials
          gen
          ~f:(f_and_enqueue return.return);
        `Cannot_generate)
    with
    | `Can_generate    -> ()
    | `Cannot_generate ->
      match sexp_of with
      | None -> failwith "cannot generate"
      | Some sexp_of_value ->
        Error.raise_s
          [%message
            "cannot generate"
              ~attempts:(!r : value list)]
end

include Configure (struct
    let default_seed = `Deterministic "an arbitrary but deterministic string"
    let default_trial_count =
      match Word_size.word_size with
      | W64 -> 10_000
      | W32 ->  1_000
    let default_can_generate_trial_count = 10_000
    let default_shrink_attempts = `Limit 1000
    let default_sizes =
      Sequence.cycle_list_exn (List.range 0 30 ~stop:`inclusive)
  end)

module type S     = S
module type S1    = S1
module type S2    = S2
module type S_int = S_int

type nonrec seed            = seed
type nonrec shrink_attempts = shrink_attempts

module type Quickcheck_config     = Quickcheck_config
module type Quickcheck_configured = Quickcheck_configured
