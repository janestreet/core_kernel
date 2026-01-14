open! Core
open! Import

module Cpuset = struct
  include Validated.Make (struct
      type t = Int.Set.t [@@deriving sexp]

      let%template[@alloc a = stack] sexp_of_t =
        (Set.sexp_of_m__t [@alloc a]) (module Int)
      ;;

      let here = [%here]

      let validate t =
        Validate.first_failure
          (Int.validate_lbound ~min:(Incl 1) (Set.length t))
          (Set.to_list t
           |> List.map ~f:Int.validate_non_negative
           |> Validate.name_list "Thread_pool_cpuset")
      ;;
    end)

  let equal t1 t2 = Int.Set.equal (t1 |> raw) (t2 |> raw)
end

type t =
  | Inherit
  | Cpuset of Cpuset.t
[@@deriving sexp]
