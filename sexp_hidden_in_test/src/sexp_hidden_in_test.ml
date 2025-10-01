open! Core.Core_stable

module Make (M : sig
    val am_running_test : bool
  end) =
struct
  type 'a t = 'a
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, stable_witness]

  let sexp_of_t sexp_of_a a =
    if M.am_running_test then Core.Sexp.Atom "<hidden_in_test>" else sexp_of_a a
  ;;

  module With_non_roundtripping_in_test_of_sexp = struct
    type nonrec 'a t = 'a t
    [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, stable_witness]
  end
end

module%test _ = struct
  module Turned_off = struct
    module Sexp_hidden_in_test_turned_off = Make (struct
        let am_running_test = false
      end)

    type nonrec t = int Sexp_hidden_in_test_turned_off.t [@@deriving sexp_of]
  end

  module Turned_on = struct
    module Sexp_hidden_in_test_turned_on = Make (struct
        let am_running_test = true
      end)

    type nonrec t = int Sexp_hidden_in_test_turned_on.t [@@deriving sexp_of]
  end

  let%expect_test "Turned on" =
    Core.print_s [%sexp (1024 : Turned_on.t)];
    [%expect {| <hidden_in_test> |}]
  ;;

  let%expect_test "Turned off" =
    Core.print_s ([%sexp_of: Turned_off.t] 1024);
    [%expect {| 1024 |}]
  ;;
end

include Make (struct
    let am_running_test = Core.am_running_test
  end)
