module List = ListLabels
module Sexp = Sexplib.Sexp
module Gen  = Raw_quickcheck_generator

open Sexplib.Std

module Sexpable_fn = struct

  type ('a, 'b) t = ('a -> 'b) * (unit -> Sexp.t)

  let create ~f ~f_sexp = f, f_sexp

  let const x ~sexp_of =
    create
      ~f:(fun _ -> x)
      ~f_sexp:(fun () ->
        <:sexp_of< [`const of Sexp.t] >>
          (`const (sexp_of x)))

  let cases ~a:(f_a, mk_sexp_a) ~b:(f_b, mk_sexp_b) =
    create
      ~f:(function `A a -> f_a a | `B b -> f_b b)
      ~f_sexp:(fun () ->
        <:sexp_of< [`cases of [`A of Sexp.t] * [`B of Sexp.t]] >>
          (`cases (`A (mk_sexp_a ()), `B (mk_sexp_b ()))))

  let compose ~fst:(f1, mk_sexp1) ~snd:(f2, mk_sexp2) =
    create
      ~f:(fun x -> f2 (f1 x))
      ~f_sexp:(fun () ->
        <:sexp_of< [`compose_left_to_right of Sexp.t * Sexp.t] >>
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
        <:sexp_of< [`lift_to_fst of Sexp.t] >>
          (`lift_to_fst (mk_sexp ())))

end

(* Tail-recursive reimplementation of [List.mapi]. *)
let list_mapi list ~f =
  List.rev (snd (List.fold_left list ~init:(0, []) ~f:(fun (i, rev_acc) x ->
    (i + 1, f i x :: rev_acc))))

TEST_UNIT "list_mapi" =
  <:test_result< int list >>
    (list_mapi [ 2 ; 7 ; 1 ; 8 ; 2 ; 8 ] ~f:(fun i x -> i * x))
    ~expect:[ 0 ; 7 ; 2 ; 24 ; 8 ; 40 ]

(* [t] represents a family of decision trees used to observe properties of some input
   type.  The [observe] function randomly chooses a single decision tree from the family
   represented by a [t].  It does so by choosing a subset of the available observations on
   a type, limiting the size of the final tree to a specific number of nodes.

   [Singleton] is a leaf in the decision tree; it makes no further decisions.

   [Variant2 (t_a, t_b)] is a node in the decision tree.  It distinguishes between [`A a]
   and [`B b].  Further observations are made using [t_a] to observe [a] or [t_b] to
   observe [b].

   [Random alist] combines multiple decision trees nondeterministically.  When
   constructing the combined decision tree, individual decision tree nodes are chosen from
   [alist] with probability proportional to their given weights.  Invariant: [alist] never
   includes [Singleton].

   [Unmap (t, fn)] wraps a node in a decision tree with intermediate work, such as mapping
   a value to a variant for use with [Variant2].  [fn] performs the conversion and [t]
   observes the output of that conversion.  Invariant: [t] is never [Singleton].

   [Fn (p, gen, t, sexp_of)] produces decision tree nodes for observing a function by
   randomly generating inputs for the function from [gen] and then observing the
   function's output using [t].  If previous outputs have been generated, [observe]
   chooses a new input with probability [p] and a previous input with probability [1-p].
   Each input can be rendered using [sexp_of].  Invariant: [t] is never [Singleton].

   [Of_fun f] lazily produces a [t]; it is generally used to short-circuit values that may
   be infinite or intractably large so that they can be explored on demand. *)
type 'a t =
  | Singleton : _ t
  | Variant2  : 'a t * 'b t                              -> [ `A of 'a | `B of 'b ] t
  | Random    : (float * 'a t) list                      -> 'a t
  | Unmap     : 'b t * ('a, 'b) Sexpable_fn.t            -> 'a t
  | Fn        : float * 'a Gen.t * 'b t * ('a -> Sexp.t) -> ('a -> 'b) t
  | Of_fun    : (unit -> 'a t)                           -> 'a t

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
      List.fold_left alist ~init:0 ~f:(fun x (_, t_b) ->
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
  match classify_float wt with
  | FP_nan | FP_infinite -> failwith "Observer.weighted_union: weight is not finite"
  | _ -> if wt < 0. then failwith "Observer.weighted_union: weight is negative"

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
    [ 1., unmap a ~f:(fun (x,_,_,_,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_1st_of_6")
    ; 1., unmap b ~f:(fun (_,x,_,_,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_2nd_of_6")
    ; 1., unmap c ~f:(fun (_,_,x,_,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_3rd_of_6")
    ; 1., unmap d ~f:(fun (_,_,_,x,_,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_4th_of_6")
    ; 1., unmap e ~f:(fun (_,_,_,_,x,_) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_5th_of_6")
    ; 1., unmap f ~f:(fun (_,_,_,_,_,x) -> x) ~f_sexp:(fun () -> Sexp.Atom "get_6th_of_6")
    ]

(* [gen_uniform_between ~lo ~hi] produces numbers uniformly distributed between [lo] and
   [hi], both inclusive.  It handles natural numbers up to [max_branching_factor], and
   thus does not need to worry about overflow in arithmetic. *)
let rec gen_uniform_between ~lo ~hi =
  if lo > hi then Gen.failure      else
  if lo = hi then Gen.singleton lo else
    begin
      let lo_mid = (lo + hi) / 2 in
      let hi_mid = lo_mid + 1    in
      Gen.weighted_union
        [ float_of_int (1 + lo_mid - lo),
          Gen.of_fun (fun () -> gen_uniform_between ~lo ~hi:lo_mid)
        ; float_of_int (1 + hi - hi_mid),
          Gen.of_fun (fun () -> gen_uniform_between ~lo:hi_mid ~hi)
        ]
    end

(* [gen_ways_to_split n ~limit_a ~limit_b] produces pairs of natural numbers [a, b] such
   that [a + b = n], [a <= limit_a], and [b <= limit_b].  It handles natural numbers up to
   [max_branching_factor], and thus does not need to worry about overflow in
   arithmetic. *)
let gen_ways_to_split n ~limit_a ~limit_b =
  Gen.bind (gen_uniform_between ~lo:(n - limit_b) ~hi:limit_a) (fun a ->
    Gen.singleton (a, n-a))

(* [decision] represents a single node in a decision tree, along with [t]s representing
   the space of possible children.

   [Decide (t_a, t_b)] represents a node that distinguishes between [`A a] and [`B b],
   with child nodes chosen from [t_a] to observe [a] and child nodes chosen from [t_b] to
   observe [b].

   [Apply (fn, decision)] represents a "wrapper" for a node that first applies [fn] to an
   input, and then uses [decision] to observe the result. *)
type 'a decision =
  | Decide : 'a t * 'b t                          -> [ `A of 'a | `B of 'b ] decision
  | Apply  : ('a, 'b) Sexpable_fn.t * 'b decision -> 'a decision

(* [randomize_decision decision i alist] is used when [decision] was chosen as one
   possible decision tree node from index [i] of [alist], which is a list of [t]s with
   [float] weights.  This function re-wraps the nondeterministic choice of [alist] around
   the child nodes of [decision], substituting the child nodes of [decision] for element
   [i] of [alist]. *)
let randomize_decision (type dom) decision i alist =
  let rec loop
    : type typ . typ decision -> (typ * dom) decision
    = function
      | Apply (fn, decision) ->
        Apply (Sexpable_fn.lift_to_fst fn, loop decision)
      | Decide (t_a, t_b) ->
        let t_a' =
          weighted_union (list_mapi alist ~f:(fun j (wt, t) ->
            if j = i
            then wt, unmap_fst t_a
            else wt, unmap_snd t))
        in
        let t_b' =
          weighted_union (list_mapi alist ~f:(fun j (wt, t) ->
            if j = i
            then wt, unmap_fst t_b
            else wt, unmap_snd t))
        in
        Apply (Sexpable_fn.lift_variant_from_fst (), Decide (t_a', t_b'))
  in
  Apply (Sexpable_fn.duplicate (), loop decision)

(* [decide t] produces a random generator of all initial [`A _]-versus-[`B _] decisions
   that [t] can make. *)
let rec decide : type dom . dom t -> dom decision Gen.t = function
  | Of_fun f            -> decide (f ())
  | Singleton           -> Gen.failure
  | Variant2 (t_a, t_b) -> Gen.singleton (Decide (t_a, t_b))
  | Unmap (t, fn) ->
    Gen.bind (decide t) (fun decision ->
      Gen.singleton (Apply (fn, decision)))
  | Random alist ->
    let gen =
      Gen.weighted_union
        (list_mapi alist ~f:(fun i (wt, t) ->
           (wt, Gen.singleton (i, t))))
    in
    Gen.bind gen (fun (i, t) ->
      Gen.bind (decide t) (fun decision ->
        Gen.singleton (randomize_decision decision i alist)))
  | Fn (p, gen, t, sexp_of) ->
    Gen.bind_choice gen (fun choice ->
      (* We don't want to repeat this choice of values again, so we strip out this choice
         of input from [gen].  We do want to allow different orders of the same inputs,
         however; see comment in .mli about "intensionally unique" functions. *)
      let dom  = Gen.Choice.value choice in
      let gen' = Gen.Choice.updated_gen choice ~keep:`All_choices_except_this_choice in
      let t' =
        unmap t ~f:(fun f -> f dom)
          ~f_sexp:(fun () ->
            <:sexp_of< [`apply_to of Sexp.t] >>
              (`apply_to (sexp_of dom)))
      in
      let alist =
        [ 1. -. p, t'
        ;       p, Fn (p, gen', t, sexp_of)
        ]
      in
      Gen.bind (decide t') (fun decision ->
        Gen.singleton (randomize_decision decision 0 alist)))

module type S = sig
  type t with sexp_of
  val gen : t Gen.t
end

module Make (T : S) : sig

  val fn_gen_of_t
    :  'a t
    -> branching_factor:int
    -> ('a, T.t) Sexpable_fn.t Gen.t

end = struct

  let rec fn_gen_of_t
    : type dom .
      (dom t -> branching_factor : int -> (dom, T.t) Sexpable_fn.t Gen.t)
    = fun t ~branching_factor ->
      if branching_factor = 0
      then
        Gen.bind T.gen (fun x ->
          Gen.singleton (Sexpable_fn.const x ~sexp_of:<:sexp_of< T.t >>))
      else
        Gen.bind (decide t) (fun decision ->
          fn_gen_of_decision decision ~branching_factor)

  and fn_gen_of_decision
    : type dom .
      (dom decision -> branching_factor : int -> (dom, T.t) Sexpable_fn.t Gen.t)
    = fun decision ~branching_factor ->
      match decision with
      | Apply (fn1, decision) ->
        Gen.bind (fn_gen_of_decision decision ~branching_factor) (fun fn2 ->
          Gen.singleton (Sexpable_fn.compose ~fst:fn1 ~snd:fn2))
      | Decide (t_a, t_b) ->
        let limit = branching_factor - 1 in
        let limit_a = limited_branching_factor t_a ~limit in
        let limit_b = limited_branching_factor t_b ~limit in
        Gen.bind (gen_ways_to_split limit ~limit_a ~limit_b) (fun (n_a, n_b) ->
          Gen.bind (fn_gen_of_t t_a ~branching_factor:n_a) (fun fn_a ->
            Gen.bind (fn_gen_of_t t_b ~branching_factor:n_b) (fun fn_b ->
              Gen.singleton (Sexpable_fn.cases ~a:fn_a ~b:fn_b))))

end

let observe (type rng) t gen ~sexp_of_rng ~branching_factor =
  if branching_factor < 0
  then failwith "Observer.observe: negative branching factor"
  else if branching_factor > max_branching_factor
  then Gen.failure
  else
    let module T = struct type t = rng with sexp_of let gen = gen end in
    let module M = Make (T) in
    M.fn_gen_of_t t ~branching_factor
