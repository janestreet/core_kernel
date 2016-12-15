open! Import

module Array = Core_array
module List  = Core_list

type t = char [@@deriving typerep]

include (Base.Char
         : module type of struct include Base.Char end
         with type t := t)

include Identifiable.Extend (Base.Char) (struct
    type t = char [@@deriving bin_io]
  end)

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  let gen_matching_memoized f =
    (* Contrary to the usual advice for [Generator.of_fun], we use memoization here.  We
       don't want to compute all of the char-category generators in every program, but any
       individual generator that is used should be computed just once.  This doesn't cause
       the memory-leak problems that can arise from memoized generators because the
       maximum total space used is a small constant. *)
    let lazy_gen =
      lazy
        (Generator.of_list
           (List.filter ~f (Array.to_list (Array.init 256 ~f:of_int_exn))))
    in
    Generator.of_fun (fun () ->
      Lazy.force lazy_gen)

  let gen_uppercase          = gen_matching_memoized is_uppercase
  let gen_lowercase          = gen_matching_memoized is_lowercase
  let gen_digit              = gen_matching_memoized is_digit
  let gen_whitespace         = gen_matching_memoized is_whitespace
  let gen_alpha              = gen_matching_memoized is_alpha
  let gen_alphanum           = gen_matching_memoized is_alphanum
  let gen_print              = gen_matching_memoized is_print
  let gen                    = gen_matching_memoized (fun _ -> true)

  let obs =
    Observer.enum 256
      ~f:to_int

  let%test_module "generators" =
    (module struct

      let all = Set.of_array (Array.init 256 ~f:of_int_exn)

      let test gen ~f =
        (* repeat to make sure changing random seed doesn't affect the outcome *)
        for _ = 1 to 10 do
          let expect = Set.filter all ~f in
          let actual =
            Sequence.delayed_fold (Quickcheck.random_sequence gen)
              ~init:Set.empty
              ~finish:Fn.id
              ~f:(fun set t ~k ->
                let set = Set.add set t in
                if Set.equal set expect
                then set
                else k set)
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
