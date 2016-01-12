include Core_list0

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Monad_infix

  let gen elem_gen ~lo ~hi keep =
    let rec loop elem_gen len tail =
      let weight_for_len  = if lo <= len && len <= hi then 1. else 0. in
      let weight_for_more = if              len <  hi then 1. else 0. in
      Generator.weighted_union
        [ weight_for_len,  Generator.singleton tail
        ; weight_for_more, Generator.bind_choice elem_gen (fun choice ->
            let elem = Generator.Choice.value choice in
            loop (Generator.Choice.updated_gen choice ~keep) (len+1) (elem::tail))
        ]
    in
    loop elem_gen 0 []

  let sort_by by t =
    match by with
    | `Arbitrarily -> t
    | `By cmp      -> Generator.map t ~f:(List.sort ~cmp)

  let gen' ?(length = `At_least 0) ?(unique = false) ?sorted elem_gen =
    let lo, hi =
      match length with
      | `Exactly           n      -> n, n
      | `At_least          n      -> n, Pervasives.max_int
      | `At_most           n      -> 0, n
      | `Between_inclusive (x, y) -> x, y
    in
    if lo < 0 || lo > hi then failwith "Generator.list: invalid length argument";
    match unique, sorted with
    | false, None    -> gen elem_gen ~lo ~hi `All_choices
    | true,  None    -> gen elem_gen ~lo ~hi `All_choices_except_this_choice
    | false, Some by -> gen elem_gen ~lo ~hi `This_choice_and_all_choices_to_the_left
                        |> sort_by by
    | true,  Some by -> gen elem_gen ~lo ~hi `Choices_to_the_left_of_this_choice_only
                        |> sort_by by

  let gen elem_gen =
    gen' elem_gen

  let rec gen_permutations list =
    match list with
    | []        -> Generator.singleton []
    | x :: list ->
      gen_permutations list
      >>= fun list ->
      Quickcheck.For_int.gen_between
        ~lower_bound:(Incl 0)
        ~upper_bound:(Incl (length list))
      >>| fun index ->
      let prefix, suffix = split_n list index in
      prefix @ [ x ] @ suffix

  let obs elem_obs =
    Observer.recursive (fun t_obs ->
      Observer.unmap
        (Observer.variant2
           (Observer.singleton ())
           (Observer.tuple2 elem_obs t_obs))
        ~f:(function
          | []        -> `A ()
          | x :: list -> `B (x, list))
        ~f_sexp:(fun () -> Atom "variant_of_list"))

  let shrinker t_elt =
    Shrinker.recursive (fun t_list ->
      Shrinker.create (function
        | []    -> Sequence.empty
        | h::tl ->
          let open Sequence.Monad_infix in
          let dropped     = Sequence.singleton tl in
          let shrunk_head = Shrinker.shrink t_elt   h >>| fun shr_h  -> shr_h::tl in
          let shrunk_tail = Shrinker.shrink t_list tl >>| fun shr_tl -> h::shr_tl in
          Sequence.interleave
            (Sequence.of_list [dropped; shrunk_head; shrunk_tail])))

  let%test_module "shrinker" =
    (module struct

      open Sexplib.Std
      module Sexp = Sexplib.Sexp

      let t0 =
        Shrinker.create (fun v ->
          if Pervasives.(=) 0 v
          then Sequence.empty
          else Sequence.singleton 0)

      let test_list = [1;2;3]
      let expect =
        [[2;3]; [0;2;3]; [1;3]; [1;0;3]; [1;2]; [1;2;0]]
        |> List.sort ~cmp:[%compare: int list ]

      let%test_unit "shrinker produces expected outputs" =
        let shrunk =
          Shrinker.shrink (shrinker t0) test_list
          |> Sequence.to_list
          |> List.sort ~cmp:[%compare: int list ]
        in
        [%test_result: int list list] ~expect shrunk

      let rec recursive_list = 1::5::recursive_list

      let%test_unit "shrinker on infinite lists produces values" =
        let shrunk = Shrinker.shrink (shrinker t0) recursive_list in
        let result_length = Sequence.take shrunk 5 |> Sequence.to_list |> List.length in
        [%test_result: int] ~expect:5 result_length
    end)

end

let gen              = For_quickcheck.gen
let gen'             = For_quickcheck.gen'
let gen_permutations = For_quickcheck.gen_permutations
let obs              = For_quickcheck.obs
let shrinker         = For_quickcheck.shrinker
