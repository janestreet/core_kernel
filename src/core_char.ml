open! Import

module Array = Core_array
module List  = Core_list

type t = char [@@deriving typerep]

module Z =
  Identifiable.Extend (Base.Char) (struct
    type t = char [@@deriving bin_io]
  end)

include (Z : module type of struct include Z end
         with module Replace_polymorphic_compare := Z.Replace_polymorphic_compare)

(* include [Base.Char] after the application of [Identifiable.Extend] to replace the [Comparable]
   functions with the pervasive versions *)
include (Base.Char
         : module type of struct include Base.Char end
         with type t := t)

module Replace_polymorphic_compare = Base.Char

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  let gen_range lo hi =
    Generator.create (fun ~size:_ random ->
      Splittable_random.int random ~lo:(to_int lo) ~hi:(to_int hi)
      |> unsafe_of_int)

  let gen_uppercase     = gen_range 'A' 'Z'
  let gen_lowercase     = gen_range 'a' 'z'
  let gen_digit         = gen_range '0' '9'
  let gen_print_uniform = gen_range ' ' '~'

  let gen_uniform = gen_range min_value max_value

  let gen_alpha    = Generator.union [ gen_lowercase ; gen_uppercase ]
  let gen_alphanum =
    Generator.weighted_union
      (* Most people probably expect this to be a uniform distribution, not weighted
         toward digits like we would get with [Generator.union] (since there are fewer
         digits than letters). *)
      [ 52., gen_alpha
      ; 10., gen_digit
      ]

  let gen_whitespace = Generator.of_list (List.filter all ~f:is_whitespace)

  let gen_print =
    Generator.weighted_union
      [ 10., gen_alphanum
      ;  1., gen_print_uniform
      ]

  let gen =
    Generator.weighted_union
      [ 10., gen_print
      ;  1., gen_uniform
      ]

  let obs =
    Observer.create (fun t ~size:_ hash ->
      [%hash_fold: t] hash t)

  let%test_module "generators" =
    (module struct

      let all = Set.of_array (Array.init 256 ~f:of_int_exn)

      let test gen ~f =
        (* repeat to make sure changing random seed doesn't affect the outcome *)
        for _ = 1 to 10 do
          let expect = Set.filter all ~f in
          let actual =
            let set = ref Set.empty in
            with_return (fun return ->
              Sequence.iter (Quickcheck.random_sequence gen) ~f:(fun t ->
                set := Set.add !set t;
                if Set.equal !set expect then return.return ()));
            !set
          in
          [%test_result: Set.t] actual ~expect
        done

      (* exported generators: *)
      let%test_unit "default"    = test gen            ~f:(fun _ -> true)
      let%test_unit "digit"      = test gen_digit      ~f:is_digit
      let%test_unit "lowercase"  = test gen_lowercase  ~f:is_lowercase
      let%test_unit "uppercase"  = test gen_uppercase  ~f:is_uppercase
      let%test_unit "alpha"      = test gen_alpha      ~f:is_alpha
      let%test_unit "alphanum"   = test gen_alphanum   ~f:is_alphanum
      let%test_unit "print"      = test gen_print      ~f:is_print
      let%test_unit "whitespace" = test gen_whitespace ~f:is_whitespace

    end)

  let shrinker =
    Shrinker.empty ()

end

let obs            = For_quickcheck.obs
let gen            = For_quickcheck.gen
let gen_digit      = For_quickcheck.gen_digit
let gen_lowercase  = For_quickcheck.gen_lowercase
let gen_uppercase  = For_quickcheck.gen_uppercase
let gen_alpha      = For_quickcheck.gen_alpha
let gen_alphanum   = For_quickcheck.gen_alphanum
let gen_print      = For_quickcheck.gen_print
let gen_whitespace = For_quickcheck.gen_whitespace
let shrinker       = For_quickcheck.shrinker
