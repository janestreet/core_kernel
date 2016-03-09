open Std_internal

module type S = sig
  include Identifiable with type t = private string

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io, compare]

      include Stable_containers.Comparable.V1.S
        with type key := t
        with type comparator_witness := comparator_witness
    end
  end
end

module Make_without_pretty_printer (M : sig val module_name : string end) () = struct
  module Stable = struct
    module V1 = struct
      include String

      let check =
        let invalid s reason =
          Error (sprintf "'%s' is not a valid %s because %s" s M.module_name reason)
        in
        fun s ->
          let len = String.length s in
          if Int.(=) len 0
          then invalid s "it is empty"
          else if Char.is_whitespace s.[0] || Char.is_whitespace s.[len-1]
          then invalid s "it has whitespace on the edge"
          else Ok ()
      ;;

      let of_string s =
        match check s with
        | Ok () -> s
        | Error err -> invalid_arg err

      let t_of_sexp sexp =
        let s = String.t_of_sexp sexp in
        match check s with
        | Ok () -> s
        | Error err -> of_sexp_error err sexp
    end
  end

  include Stable.V1

end

let%test_module _ = (module struct
  module M =
    Make_without_pretty_printer (struct
      let module_name = "test"
    end) ()

  let%test_unit "String_id's of_string shouldn't allocate on success" =
    let initial_words = Core_gc.minor_words () in
    ignore (M.of_string "FOOBAR");
    let allocated = (Core_gc.minor_words ()) - initial_words in
    [%test_result: int] allocated ~expect:0
  ;;
end)

module Make (M : sig val module_name : string end) () = struct
  include Make_without_pretty_printer(M) ()

  include Pretty_printer.Register (struct
      type nonrec t = t
      let module_name = M.module_name
      let to_string = to_string
    end)
end

include Make (struct let module_name = "Core_kernel.String_id" end) ()

let%bench_module "String_id" = (module struct
  let%bench "of_string(AAA)"       = of_string "AAA"
  let%bench "of_string(AAABBB)"    = of_string "AAABBB"
  let%bench "of_string(AAABBBCCC)" = of_string "AAABBBCCC"
end)
