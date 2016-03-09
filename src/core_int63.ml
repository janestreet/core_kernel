#import "config.mlh"
module type Int_or_more = sig
  include Int_intf.S
  val of_int : int -> t
  val to_int : t -> int option
end
#if JSC_ARCH_SIXTYFOUR
include
  (struct include Core_int let to_int x = Some x end
   : Int_or_more with type t = private int)
#else
include (Core_int64 : Int_or_more)
#endif

module Overflow_exn = struct
  let ( + ) t u =
    let sum = t + u in
    if bit_or (bit_xor t u) (bit_xor t (bit_not sum)) < zero
    then sum
    else Common.failwiths "( + ) overflow" (t, u, sum) [%sexp_of: t * t * t]
  ;;
  let%test_module "( + )" =
    (module struct
      let test t = Exn.does_raise (fun () -> t + t)
      let%test "max_value / 2 + 1"     = test (succ (max_value / of_int 2))
      let%test "min_value / 2 - 1"     = test (pred (min_value / of_int 2))
      let%test "min_value + min_value" = test min_value
      let%test "max_value + max_value" = test max_value
    end)
  ;;
  let ( - ) t u =
    let diff = t - u in
    let pos_diff = t > u in
    if t <> u && Bool.(<>) pos_diff (is_positive diff) then
      Common.failwiths "( - ) overflow" (t, u, diff) [%sexp_of: t * t * t]
    else diff
  ;;
  let%test_module "( - )" =
    (module struct
      let%test "min_value -  1" = Exn.does_raise (fun () -> min_value -     one)
      let%test "max_value - -1" = Exn.does_raise (fun () -> max_value - neg one)
      let%test "min_value / 2 - max_value / 2 - 2" =
        Exn.does_raise (fun () -> min_value / of_int 2 - max_value / of_int 2 - of_int 2)
      let%test "min_value - max_value" = Exn.does_raise (fun () -> min_value - max_value)
      let%test "max_value - min_value" = Exn.does_raise (fun () -> max_value - min_value)
      let%test "max_value - -max_value" =
        Exn.does_raise (fun () -> max_value - neg max_value)
    end)
  ;;

  let abs t = if t = min_value then failwith "abs overflow" else abs t
  let neg t = if t = min_value then failwith "neg overflow" else neg t
end

let () = assert (Core_int.(>=) num_bits 63)

(* Even for ARCH_SIXTYFOUR, we can't use Core_random.State.int, because the bound is very
   limited in range.  We actually want a bound that spans the type. *)
let random ?(state = Core_random.State.default) bound =
  of_int64_exn (Core_random.State.int64 state (to_int64 bound))
;;
