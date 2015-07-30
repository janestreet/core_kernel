open Std_internal

module Generator = Raw_quickcheck_generator
module Observer  = Raw_quickcheck_observer

include Generator
include Monad.Make (struct
    include Generator
    let return = singleton
    let map = `Define_using_bind
  end)

type 'a obs = 'a Observer.t

type ('a, 'b) fn_with_sexp = ('a -> 'b) * (unit -> Sexp.t)

let fn_sexp (_, mk_sexp) = mk_sexp ()

let sexp_of_fn_with_sexp _ _ fn = fn_sexp fn

type nan_dist = Without | With_single | With_all

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

let singleton = return

let doubleton x y = union [ singleton x ; singleton y ]

let partitions_of_list list ~max_partition_size =
  List.groupi list ~break:(fun i _ _ -> i mod max_partition_size = 0)

TEST_UNIT =
  for len = 0 to 50 do
    let list = List.init len ~f:Fn.id in
    for max_partition_size = 1 to len do
      let partitions = partitions_of_list list ~max_partition_size in
      <:test_result< int >>
        ~message:(sprintf "len = %d, max_partition_size = %d" len max_partition_size)
        (List.fold partitions ~init:0 ~f:(fun max_len_so_far partition ->
           max max_len_so_far (List.length partition)))
        ~expect:max_partition_size
    done
  done

TEST_UNIT =
  for len = 0 to 50 do
    let list = List.init len ~f:Fn.id in
    for max_partition_size = 1 to len do
      let partitions = partitions_of_list list ~max_partition_size in
      <:test_result< int list >>
        ~message:(sprintf "len = %d, max_partition_size = %d" len max_partition_size)
        (List.concat partitions)
        ~expect:list
    done
  done

let rec of_list list =
  let branching_factor = 16 in
  let len = List.length list in
  if len <= branching_factor
  then union (List.map list ~f:singleton)
  else
    let max_partition_size =
      Int.round_up len ~to_multiple_of:branching_factor
      / branching_factor
    in
    partitions_of_list list ~max_partition_size
    |> List.map ~f:(fun list -> (Float.of_int (List.length list), of_list list))
    |> weighted_union

let of_sequence ?(p = Percent.of_mult 0.25) seq =
  let p = Percent.to_mult p in
  if Float.( < ) p 0. || Float.( > ) p 1.
  then failwithf "Generator.of_sequence: probability [%f] out of bounds" p ();
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

let count_bits_non_negative (type t) (module I : Int_intf.S with type t = t) (t : t) =
  let open I in
  if t < zero
  then failwith "count_bits_non_negative: input is negative";
  let n = ref 0 in
  let t = ref t in
  while !t > zero do
    Int.incr n;
    t := shift_right_logical !t 1
  done;
  !n

TEST_MODULE "count_bits_non_negative" = struct
  let check x num_bits =
    <:test_result<int>>
      (count_bits_non_negative (module Int) x)
      ~expect:num_bits
      ~message:(sprintf "bits of %s" (Int.to_string_hum x))

  TEST_UNIT = check 4 3;
  TEST_UNIT = check 5 3;
  TEST_UNIT = check 8 4;
  TEST_UNIT "log based" =
    let lg x = log x /. log 2. in
    let expect n = Int.succ (Float.iround_exn ~dir:`Down (lg (Float.of_int n))) in
    for i = 1 to 10_000 do
      check i (expect i)
    done
  ;;
end

module Make_int_generator (M : Int_intf.S) : sig
  val t_gen : M.t t
  val t_gen_between
    :  lower_bound:M.t Comparable.bound
    -> upper_bound:M.t Comparable.bound
    -> M.t t
end = struct

  open M

  let average_rounded_down x y =
    (shift_right x 1) + (shift_right y 1) + (bit_and (bit_and x y) one)

  TEST_UNIT "average_rounded_down" =
    let check here x y ~expect =
      let actual = average_rounded_down x y in
      <:test_result<M.t>> ~here:[here] actual ~expect
    in
    let check_int here x y =
      check here (of_int_exn x) (of_int_exn y)
        ~expect:(of_int_exn Int.((x + y) / 2))
    in
    check_int _here_ 3 5;
    check_int _here_ 4 6;
    check_int _here_ 4 5;
    check _here_ min_value max_value ~expect:(- one);
    check _here_ max_value min_value ~expect:(- one);
    check _here_ max_value max_value ~expect:max_value;
    check _here_ min_value min_value ~expect:min_value

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

  TEST_UNIT "ranges_by_magnitude_and_sign exhaustive" =
    let low = (-100) in
    let high = 100 in
    let lower_bound = of_int_exn low and upper_bound = of_int_exn high in
    let ranges =
      ranges_by_magnitude_and_sign ~lower_bound ~upper_bound
      |> List.map ~f:(fun (a, b) -> to_int_exn a, to_int_exn b)
    in
    let mem n =
      List.exists ranges ~f:(fun (lower, upper) ->
        Int.(<=) lower n && Int.(<=) n upper)
    in
    assert (not (mem (Int.pred low)));
    assert (not (mem (Int.succ high)));
    for i = low to high do assert (mem i) done;
  ;;

  TEST_UNIT "ranges_by_magnitude_and_sign num_ranges grows slowly" =
    let m = match Word_size.word_size with W32 -> 29 | W64 -> 32 in
    for i = 0 to m do
      let n = Int.pow 2 i in
      let num_ranges =
        ranges_by_magnitude_and_sign ~lower_bound:zero ~upper_bound:(of_int_exn n)
        |> List.length
      in
      assert (Int.(num_ranges <= 2 * (i + 1)))
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

  let t_gen_uniform ~lower_bound ~upper_bound =
    snd (weighted_uniform ~lower_bound ~upper_bound)

  let t_gen_between_inclusive ~lower_bound ~upper_bound =
    if lower_bound > upper_bound
    then failure
    else
      (* [ranges] is a list of tuples representing inclusive lower and upper bounds of
         disjoint ranges that add up to the entirety of [lower_bound, upper_bound].  These
         ranges are constructed to start at size 1 at the boundaries and approximately
         double in size as they approach the middle of the range.  Each range is converted
         into a uniform distribution of values.

         The final generator is constructed as a union of these ranges, weighted in
         inverse proportion to the log of their sizes.  The intention is to consistently
         exercise boundary conditions, while still leaving a fair probability of choosing
         arbitrary values out of the middle of the distribution. *)
      let ranges = ranges_by_magnitude_and_sign ~lower_bound ~upper_bound in
      weighted_union (List.map ranges ~f:(fun (lower, upper) ->
        let inverse_wt =
          count_bits_non_negative (module M) (succ (abs (upper - lower)))
        in
        1. /. Float.of_int inverse_wt,
        t_gen_uniform ~lower_bound:lower ~upper_bound:upper))

  let t_gen_between ~lower_bound ~upper_bound =
    match (lower_bound : t Comparable.bound), (upper_bound : t Comparable.bound) with
    | Excl lower, _ when lower = max_value -> failure
    | _, Excl upper when upper = min_value -> failure
    | _ ->
      let lower_bound =
        match lower_bound with
        | Unbounded             -> min_value
        | Incl lower_bound -> lower_bound
        | Excl lower_bound -> lower_bound + one
      in
      let upper_bound =
        match upper_bound with
        | Unbounded             -> max_value
        | Incl upper_bound -> upper_bound
        | Excl upper_bound -> upper_bound - one
      in
      t_gen_between_inclusive ~lower_bound ~upper_bound

  let t_gen = t_gen_between ~lower_bound:Unbounded ~upper_bound:Unbounded

end

module Int_gen = Make_int_generator (Int)
let int_between = Int_gen.t_gen_between
let int         = Int_gen.t_gen

module Int63_gen = Make_int_generator (Core_int63)
let int63_between = Int63_gen.t_gen_between

let recursive f =
  let rec self () = f (of_fun self) in
  of_fun self

let either a b =
  union [ map a ~f:(fun a -> First  a)
        ; map b ~f:(fun b -> Second b)
        ]

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

let unit = singleton ()
let bool = doubleton true false

let gen_list elem_t ~lo ~hi keep =
  let rec loop elem_t len tail =
    let weight_for_len  = if lo <= len && len <= hi then 1. else 0. in
    let weight_for_more = if              len <  hi then 1. else 0. in
    weighted_union
      [ weight_for_len,  singleton tail
      ; weight_for_more, bind_choice elem_t (fun choice ->
          let elem = Choice.value choice in
          loop (Choice.updated_gen choice ~keep) (len+1) (elem::tail))
      ]
  in
  loop elem_t 0 []

let sort by t =
  match by with
  | `Arbitrarily -> t
  | `By cmp -> map t ~f:(List.sort ~cmp)

let list ?(length = `At_least 0) ?(unique = false) ?sorted t =
  let lo, hi =
    match length with
    | `Exactly           n      -> n, n
    | `At_least          n      -> n, Int.max_value
    | `At_most           n      -> 0, n
    | `Between_inclusive (x, y) -> x, y
  in
  if lo < 0 || lo > hi then failwith "Generator.list: invalid length argument";
  match unique, sorted with
  | false, None    -> gen_list t ~lo ~hi `All_choices
  | true,  None    -> gen_list t ~lo ~hi `All_choices_except_this_choice
  | false, Some by -> gen_list t ~lo ~hi `This_choice_and_all_choices_to_the_left |> sort by
  | true,  Some by -> gen_list t ~lo ~hi `Choices_to_the_left_of_this_choice_only |> sort by

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

let option t =
  union
    [ singleton None
    ; t >>| Option.return
    ]

let rec permute list =
  match list with
  | [] -> singleton []
  | x :: list ->
    permute list
    >>= fun list ->
    int_between
      ~lower_bound:(Incl 0)
      ~upper_bound:(Incl (List.length list))
    >>| fun index ->
    let prefix, suffix = List.split_n list index in
    prefix @ [ x ] @ suffix

let chars_matching f =
  of_fun (fun () -> of_list (List.filter ~f (List.init 256 ~f:Char.of_int_exn)))

let char_is_punctuation c =
  Char.is_print c
  && not (Char.is_alphanum c)
  && not (Char.is_whitespace c)

let char_is_non_print c =
  not (Char.is_print c)
  && not (Char.is_whitespace c)

let char_uppercase   = chars_matching Char.is_uppercase
let char_lowercase   = chars_matching Char.is_lowercase
let char_digit       = chars_matching Char.is_digit
let char_whitespace  = chars_matching Char.is_whitespace
let char_alpha       = chars_matching Char.is_alpha
let char_alphanum    = chars_matching Char.is_alphanum
let char_punctuation = chars_matching char_is_punctuation
let char_non_print   = chars_matching char_is_non_print

let char_print =
  weighted_union
    [ 10., char_alphanum
    ;  1., char_punctuation
    ;  1., char_whitespace
    ]

let char =
  weighted_union
    [ 10., char_print
    ;  1., char_non_print
    ]

let string_of char_t =
  list char_t
  >>| String.of_char_list

let string = string_of char

let sexp = recursive (fun sexp_t ->
  variant2 string (list sexp_t)
  >>| function
  | `A atom -> Sexp.Atom atom
  | `B list -> Sexp.List list)

let rec int63_pair_lexicographic
          ~fst_lower_bound ~fst_upper_bound
          ~snd_lower_bound ~snd_upper_bound
          ~snd_start ~snd_final
  =
  let open Core_int63 in
  if fst_lower_bound > fst_upper_bound
  || snd_lower_bound > snd_upper_bound
  then failure
  else if fst_lower_bound = fst_upper_bound
  then
    let fst = fst_lower_bound in
    int63_between
      ~lower_bound:(Incl snd_start)
      ~upper_bound:(Incl snd_final)
    >>| fun snd ->
    fst, snd
  else if snd_start > snd_lower_bound
  then
    let top_row =
      int63_between
        ~lower_bound:(Incl snd_start)
        ~upper_bound:(Incl snd_upper_bound)
      >>| fun snd ->
      fst_lower_bound, snd
    in
    let rest_rows =
      let fst_lower_bound = succ fst_lower_bound in
      let snd_start       =      snd_lower_bound in
      int63_pair_lexicographic
        ~fst_lower_bound ~fst_upper_bound
        ~snd_lower_bound ~snd_upper_bound
        ~snd_start ~snd_final
    in
    union [ top_row ; rest_rows ]
  else if snd_final < snd_upper_bound
  then
    let bot_row =
      int63_between
        ~lower_bound:(Incl snd_lower_bound)
        ~upper_bound:(Incl snd_final)
      >>| fun snd ->
      fst_upper_bound, snd
    in
    let rest_rows =
      let fst_upper_bound = pred fst_upper_bound in
      let snd_final = snd_upper_bound in
      int63_pair_lexicographic
        ~fst_lower_bound ~fst_upper_bound
        ~snd_lower_bound ~snd_upper_bound
        ~snd_start ~snd_final
    in
    union [ rest_rows ; bot_row ]
  else
    tuple2
      (int63_between
        ~lower_bound:(Incl fst_lower_bound)
        ~upper_bound:(Incl fst_upper_bound))
      (int63_between
        ~lower_bound:(Incl snd_lower_bound)
        ~upper_bound:(Incl snd_upper_bound))

let min_pos_normal = Float.min_positive_normal_value
let max_pos_normal = Float.max_finite_value

let min_pos_subnormal = Float.min_positive_subnormal_value
let max_pos_subnormal = min_pos_normal |> Float.one_ulp `Down

let min_normal_mantissa = Float.ieee_mantissa min_pos_normal
let max_normal_mantissa = Float.ieee_mantissa max_pos_normal

let nan_exponent = Float.ieee_exponent Float.nan
let min_nan_mantissa = min_normal_mantissa |> Core_int63.succ
let max_nan_mantissa = max_normal_mantissa

let float_nan = function
  | Without     -> failure
  | With_single -> singleton Float.nan
  | With_all    ->
    int63_between
      ~lower_bound:(Incl min_nan_mantissa)
      ~upper_bound:(Incl max_nan_mantissa)
    >>= fun mantissa ->
    bool
    >>| fun negative ->
    let exponent = nan_exponent in
    Float.create_ieee_exn ~negative ~exponent ~mantissa

let float_pos_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate =
  if lower_bound > max_pos_candidate
  || upper_bound < min_pos_candidate
  then failure
  else
    let lower = Float.max lower_bound min_pos_candidate in
    let upper = Float.min upper_bound max_pos_candidate in
    int63_pair_lexicographic
      ~fst_lower_bound:(Float.ieee_exponent lower |> Core_int63.of_int)
      ~fst_upper_bound:(Float.ieee_exponent upper |> Core_int63.of_int)
      ~snd_lower_bound:(Float.ieee_mantissa min_pos_candidate)
      ~snd_upper_bound:(Float.ieee_mantissa max_pos_candidate)
      ~snd_start:(Float.ieee_mantissa lower)
      ~snd_final:(Float.ieee_mantissa upper)
    >>| fun (exponent, mantissa) ->
    let negative = false in
    let exponent = Core_int63.to_int_exn exponent in
    Float.create_ieee_exn ~negative ~exponent ~mantissa

let float_neg_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate =
  float_pos_range
    ~lower_bound:(Float.neg upper_bound)
    ~upper_bound:(Float.neg lower_bound)
    ~min_pos_candidate
    ~max_pos_candidate
  >>| Float.neg

let float_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate =
  union
    [ float_pos_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate
    ; float_neg_range ~lower_bound ~upper_bound ~min_pos_candidate ~max_pos_candidate
    ]

let float_zero ~lower_bound ~upper_bound =
  float_range ~lower_bound ~upper_bound
    ~min_pos_candidate:Float.zero
    ~max_pos_candidate:Float.zero

let float_subnormal ~lower_bound ~upper_bound =
  float_range ~lower_bound ~upper_bound
    ~min_pos_candidate:min_pos_subnormal
    ~max_pos_candidate:max_pos_subnormal

let float_normal ~lower_bound ~upper_bound =
  float_range ~lower_bound ~upper_bound
    ~min_pos_candidate:min_pos_normal
    ~max_pos_candidate:max_pos_normal

let float_infinity ~lower_bound ~upper_bound =
  float_range ~lower_bound ~upper_bound
    ~min_pos_candidate:Float.infinity
    ~max_pos_candidate:Float.infinity

let float_between_inclusive ~nan ~lower_bound ~upper_bound =
  if Float.is_nan lower_bound
  || Float.is_nan upper_bound
  then failwith "Generator.float_between_inclusive: NaN bound"
  else if Float.(>) lower_bound upper_bound
  then failure
  else
    weighted_union
      [ 5., float_normal    ~lower_bound ~upper_bound
      ; 4., float_subnormal ~lower_bound ~upper_bound
      ; 3., float_zero      ~lower_bound ~upper_bound
      ; 2., float_infinity  ~lower_bound ~upper_bound
      ; 1., float_nan       nan
      ]

let float =
  float_between_inclusive
    ~nan:With_single
    ~lower_bound:Float.neg_infinity
    ~upper_bound:Float.infinity

let fn_with_sexp ?branching_factor dom rng ~sexp_of_rng =
  let branching_factor =
    match branching_factor with
    | Some t -> t
    | None   -> geometric () ~maximum:(Observer.branching_factor dom)
  in
  branching_factor
  >>= fun branching_factor ->
  Observer.observe ~branching_factor dom rng ~sexp_of_rng

let fn2_with_sexp ?branching_factor dom1 dom2 rng ~sexp_of_rng =
  fn_with_sexp ?branching_factor
    (Observer.tuple2 dom1 dom2)
    rng ~sexp_of_rng
  >>| fun (f, mk_sexp) ->
  (fun x1 x2 -> f (x1, x2)),
  (fun () -> <:sexp_of< [`curry of Sexp.t] >> (`curry (mk_sexp ())))

let fn3_with_sexp ?branching_factor dom1 dom2 dom3 rng ~sexp_of_rng =
  fn_with_sexp ?branching_factor
    (Observer.tuple3 dom1 dom2 dom3)
    rng ~sexp_of_rng
  >>| fun (f, mk_sexp) ->
  (fun x1 x2 x3 -> f (x1, x2, x3)),
  (fun () -> <:sexp_of< [`curry3 of Sexp.t] >> (`curry3 (mk_sexp ())))

let fn4_with_sexp ?branching_factor dom1 dom2 dom3 dom4 rng ~sexp_of_rng =
  fn_with_sexp ?branching_factor
    (Observer.tuple4 dom1 dom2 dom3 dom4)
    rng ~sexp_of_rng
  >>| fun (f, mk_sexp) ->
  (fun x1 x2 x3 x4 -> f (x1, x2, x3, x4)),
  (fun () -> <:sexp_of< [`curry4 of Sexp.t] >> (`curry4 (mk_sexp ())))

let fn5_with_sexp ?branching_factor dom1 dom2 dom3 dom4 dom5 rng ~sexp_of_rng =
  fn_with_sexp ?branching_factor
    (Observer.tuple5 dom1 dom2 dom3 dom4 dom5)
    rng ~sexp_of_rng
  >>| fun (f, mk_sexp) ->
  (fun x1 x2 x3 x4 x5 -> f (x1, x2, x3, x4, x5)),
  (fun () -> <:sexp_of< [`curry5 of Sexp.t] >> (`curry5 (mk_sexp ())))

let fn6_with_sexp ?branching_factor dom1 dom2 dom3 dom4 dom5 dom6 rng ~sexp_of_rng =
  fn_with_sexp ?branching_factor
    (Observer.tuple6 dom1 dom2 dom3 dom4 dom5 dom6)
    rng ~sexp_of_rng
  >>| fun (f, mk_sexp) ->
  (fun x1 x2 x3 x4 x5 x6 -> f (x1, x2, x3, x4, x5, x6)),
  (fun () -> <:sexp_of< [`curry6 of Sexp.t] >> (`curry6 (mk_sexp ())))

let compare_fn_with_sexp ?branching_factor dom =
  fn_with_sexp ?branching_factor dom int ~sexp_of_rng:<:sexp_of< int >>
  >>| fun (get_index, mk_sexp) ->
  (fun x y -> Int.compare (get_index x) (get_index y)),
  (fun () ->
     <:sexp_of< [`compare_using_index_fn of Sexp.t] >>
       (`compare_using_index_fn (mk_sexp ())))

let equal_fn_with_sexp ?branching_factor dom =
  compare_fn_with_sexp ?branching_factor dom
  >>| fun (cmp, mk_sexp) ->
  (fun x y -> Int.equal (cmp x y) 0),
  (fun () ->
     <:sexp_of< [`equal_fn_of_compare_fn of Sexp.t] >>
       (`equal_fn_of_compare_fn (mk_sexp ())))

let fn ?branching_factor a b =
  fn_with_sexp  ?branching_factor a b           ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn2 ?branching_factor a b c =
  fn2_with_sexp ?branching_factor a b c         ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn3 ?branching_factor a b c d =
  fn3_with_sexp ?branching_factor a b c d       ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn4 ?branching_factor a b c d e =
  fn4_with_sexp ?branching_factor a b c d e     ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn5 ?branching_factor a b c d e f =
  fn5_with_sexp ?branching_factor a b c d e f   ~sexp_of_rng:<:sexp_of< _ >> >>| fst
let fn6 ?branching_factor a b c d e f g =
  fn6_with_sexp ?branching_factor a b c d e f g ~sexp_of_rng:<:sexp_of< _ >> >>| fst

let compare_fn ?branching_factor a = compare_fn_with_sexp ?branching_factor a >>| fst
let equal_fn   ?branching_factor a = equal_fn_with_sexp   ?branching_factor a >>| fst

let float_between ~nan ~lower_bound ~upper_bound =
  match
    (lower_bound : float Comparable.bound),
    (upper_bound : float Comparable.bound)
  with
  | (Excl lower, _) | (Incl lower, _) when Float.is_nan lower ->
    failwith "Quickcheck.Generator.float_between: lower bound = NaN"
  | (_, Excl upper) | (_, Incl upper) when Float.is_nan upper ->
    failwith "Quickcheck.Generator.float_between: upper bound = NaN"
  | Excl lower, _ when Float.equal lower Float.infinity     -> float_nan nan
  | _, Excl upper when Float.equal upper Float.neg_infinity -> float_nan nan
  | _ ->
    let lower_bound =
      match lower_bound with
      | Unbounded        -> Float.neg_infinity
      | Incl lower_bound -> lower_bound
      | Excl lower_bound -> Float.one_ulp `Up lower_bound
    in
    let upper_bound =
      match upper_bound with
      | Unbounded        -> Float.infinity
      | Incl upper_bound -> upper_bound
      | Excl upper_bound -> Float.one_ulp `Down upper_bound
    in
    float_between_inclusive ~nan ~upper_bound ~lower_bound

let float_without_nan =
  float_between
    ~nan:Without
    ~lower_bound:Unbounded
    ~upper_bound:Unbounded

let float_finite =
  float_between
    ~nan:Without
    ~lower_bound:(Excl Float.neg_infinity)
    ~upper_bound:(Excl Float.infinity)
