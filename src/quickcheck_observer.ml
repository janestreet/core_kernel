open Std_internal

include Raw_quickcheck_observer

type 'a gen = 'a Raw_quickcheck_generator.t

let either a b =
  unmap (variant2 a b)
    ~f:(function
      | First  a -> `A a
      | Second b -> `B b)
    ~f_sexp:(fun () -> Sexp.Atom "variant_of_either")

let of_predicate a b ~f ~f_sexp =
  unmap (variant2 a b)
    ~f:(fun x -> if f x then `A x else `B x)
    ~f_sexp:(fun () ->
      <:sexp_of< [`variant_by_predicate of Sexp.t] >>
        (`variant_by_predicate (f_sexp ())))

let doubleton f ~f_sexp =
  of_predicate (singleton ()) (singleton ()) ~f ~f_sexp

let unit = singleton ()
let bool = doubleton Fn.id ~f_sexp:(fun () -> Sexp.Atom "id")

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

module Make_int_observer (M : Int_intf.S) : sig
  val t_obs : M.t t
  val t_obs_between
    :  lower_bound:M.t Comparable.bound
    -> upper_bound:M.t Comparable.bound
    -> M.t t
end = struct

  open M

  let bit_is_one index =
    unmap bool
      ~f:(fun t -> bit_and t (shift_left one index) <> zero)
      ~f_sexp:(fun () -> <:sexp_of< [`bit_is_one of int] >> (`bit_is_one index))

  let non_negative_up_to_inclusive upper_bound =
    let bit_count = count_bits_non_negative (module M) upper_bound in
    (* These weights are chosen to bias observations towards the most and least
       significant bits and away from the "center" bits. *)
    weighted_union
      (List.init bit_count ~f:(fun i ->
         let inverse_wt =
           Int.succ
             (count_bits_non_negative (module Int) (Int.min i (Int.( - ) bit_count i)))
         in
         1. /. Float.of_int inverse_wt,
         bit_is_one i))

  let non_negative_between_inclusive ~lower_bound ~upper_bound =
    unmap (non_negative_up_to_inclusive (upper_bound - lower_bound))
      ~f:(fun x -> x - lower_bound)
      ~f_sexp:(fun () -> <:sexp_of< [`decrement_by of t] >> (`decrement_by lower_bound))

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

  let t_obs_between_inclusive ~lower_bound ~upper_bound =
    if lower_bound >= upper_bound
    then singleton ()
    else if lower_bound >= zero
    then non_negative_between_inclusive ~lower_bound ~upper_bound
    else if upper_bound < zero
    then negative_between_inclusive ~lower_bound ~upper_bound
    else signed_between_inclusive ~lower_bound ~upper_bound

  let t_obs_between ~lower_bound ~upper_bound =
    match (lower_bound : t Comparable.bound), (upper_bound : t Comparable.bound) with
    | Excl lower, _ when lower = max_value -> singleton ()
    | _, Excl upper when upper = min_value -> singleton ()
    | _ ->
      let lower_bound =
        match lower_bound with
        | Unbounded        -> min_value
        | Incl lower_bound -> lower_bound
        | Excl lower_bound -> lower_bound + one
      in
      let upper_bound =
        match upper_bound with
        | Unbounded        -> max_value
        | Incl upper_bound -> upper_bound
        | Excl upper_bound -> upper_bound - one
      in
      t_obs_between_inclusive ~lower_bound ~upper_bound

  let t_obs =
    t_obs_between
      ~lower_bound:Unbounded
      ~upper_bound:Unbounded

end

module Int_obs = Make_int_observer (Int)
let int_between = Int_obs.t_obs_between
let int         = Int_obs.t_obs

module Int63_obs = Make_int_observer (Core_int63)
let int63_between = Int63_obs.t_obs_between

let enum n ~f ~f_sexp =
  let index =
    int_between
      ~lower_bound:(Incl 0)
      ~upper_bound:(Excl n)
  in
  unmap index ~f ~f_sexp

let findi_exn x list ~equal =
  fst (Option.value_exn (List.findi list ~f:(fun _ y -> equal x y)))

let of_list list ~equal ~sexp_of_elt =
  let f x = findi_exn x list ~equal in
  enum (List.length list) ~f
    ~f_sexp:(fun () ->
      <:sexp_of< [`find_index_in_list of elt list] >>
        (`find_index_in_list list))

let recursive f =
  let rec self () = f (of_fun self) in
  of_fun self

let option t =
  unmap (variant2 (singleton ()) t)
    ~f:(function
      | None   -> `A ()
      | Some x -> `B x)
    ~f_sexp:(fun () -> Sexp.Atom "variant_of_option")

let list t = recursive (fun list_t ->
  unmap (variant2 (singleton ()) (tuple2 t list_t))
    ~f:(function
      | []        -> `A ()
      | x :: list -> `B (x, list))
    ~f_sexp:(fun () -> Sexp.Atom "variant_of_list"))

let char = enum 256 ~f:Char.to_int ~f_sexp:(fun () -> Sexp.Atom "Char.to_int")

let string =
  unmap (list char) ~f:String.to_list ~f_sexp:(fun () -> Sexp.Atom "String.to_list")

let sexp =
  recursive (fun sexp_t ->
    unmap (variant2 string (list sexp_t))
      ~f:(function
        | Sexp.Atom atom -> `A atom
        | Sexp.List list -> `B list)
      ~f_sexp:(fun () -> Sexp.Atom "variant_of_sexp"))

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
      <:sexp_of< [`variant3_by_comparison_to of (eq * Sexp.t)] >>
        (`variant3_by_comparison_to (eq, compare_sexp ())))

let min_pos_normal = Float.min_positive_normal_value
let max_pos_normal = Float.max_finite_value

let min_normal_exponent = Float.ieee_exponent min_pos_normal
let max_normal_exponent = Float.ieee_exponent max_pos_normal

let min_normal_mantissa = Float.ieee_mantissa min_pos_normal
let max_normal_mantissa = Float.ieee_mantissa max_pos_normal

let min_pos_subnormal = Float.min_positive_subnormal_value
let max_pos_subnormal = Float.one_ulp `Down min_pos_normal

let min_subnormal_mantissa = Float.ieee_mantissa min_pos_subnormal
let max_subnormal_mantissa = Float.ieee_mantissa max_pos_subnormal

(* {zero,subnormal,normal,infinite,nan}_float are observers that distinguish
   floats already known to classify as their names suggest. E.g., [zero_float]
   only distinguishes between positive and negative zero.  *)

let zero_float =
  unmap bool ~f:Float.ieee_negative ~f_sexp:(fun () -> Sexp.Atom "Float.ieee_negative")

let subnormal_float =
  let mantissa =
    int63_between
      ~lower_bound:(Incl min_subnormal_mantissa)
      ~upper_bound:(Incl max_subnormal_mantissa)
  in
  unmap (tuple2 bool mantissa)
    ~f:(fun float ->
      Float.ieee_negative float,
      Float.ieee_mantissa float)
    ~f_sexp:(fun () -> Sexp.Atom "ieee_negative_and_mantissa")

let normal_float =
  let exponent =
    int_between
      ~lower_bound:(Incl min_normal_exponent)
      ~upper_bound:(Incl max_normal_exponent)
  in
  let mantissa =
    int63_between
      ~lower_bound:(Incl min_normal_mantissa)
      ~upper_bound:(Incl max_normal_mantissa)
  in
  unmap (tuple3 bool exponent mantissa)
    ~f:(fun float ->
      Float.ieee_negative float,
      Float.ieee_exponent float,
      Float.ieee_mantissa float)
    ~f_sexp:(fun () -> Sexp.Atom "ieee_negative_and_exponent_and_mantissa")

let infinite_float =
  unmap bool ~f:Float.ieee_negative ~f_sexp:(fun () -> Sexp.Atom "Float.ieee_negative")

let nan_float = singleton ()

let float =
  unmap (variant5 zero_float subnormal_float normal_float infinite_float nan_float)
    ~f:(fun float ->
      match Float.classify float with
      | Zero      -> `A float
      | Subnormal -> `B float
      | Normal    -> `C float
      | Infinite  -> `D float
      | Nan       -> `E float)
    ~f_sexp:(fun () -> Sexp.Atom "variant5_of_float_by_classification")
