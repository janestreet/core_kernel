open Typerep_lib.Std
open Sexplib.Std
open Bin_prot.Std
module Char  = Caml.Char
module List  = Caml.ListLabels
module Array = Caml.ArrayLabels

let failwithf = Core_printf.failwithf

module T = struct
  type t = char [@@deriving bin_io, sexp, typerep]

  let compare = Char.compare
  let hash = Hashtbl.hash

  let to_string t = String.make 1 t

  let of_string s =
    match String.length s with
    | 1 -> String.get s 0
    | _ -> failwithf "Char.of_string: %S" s ()
end
include T
include Identifiable.Make (struct
  include T
  let module_name = "Core.Std.Char"
end)

let to_int = Char.code

let unsafe_of_int = Char.unsafe_chr

(* We use our own range test when converting integers to chars rather than
   calling [Caml.Char.chr] because it's simple and it saves us a function call
   and the try-with (exceptions cost, especially in the world with backtraces). *)
let int_is_ok i =
  let open Int_replace_polymorphic_compare in
  0 <= i && i <= 255

let min_value = unsafe_of_int 0
let max_value = unsafe_of_int 255

let of_int i =
  if int_is_ok i
  then Some (unsafe_of_int i)
  else None
;;

let of_int_exn i =
  if int_is_ok i
  then unsafe_of_int i
  else failwithf "Char.of_int_exn got integer out of range: %d" i ()
;;

let%test_module "int to char conversion" =
  (module struct

    let%test_unit "of_int bounds" =
      let bounds_check i =
        [%test_result: t option]
          (of_int i)
          ~expect:None
          ~message:(string_of_int i)
      in
      for i = 1 to 100 do
        bounds_check (-i);
        bounds_check (255 + i);
      done

    let%test_unit "of_int_exn vs of_int" =
      for i = -100 to 300 do
        [%test_eq: t option]
          (of_int i)
          (Option.try_with (fun () -> of_int_exn i))
          ~message:(string_of_int i)
      done

    let%test_unit "unsafe_of_int vs of_int_exn" =
      for i = 0 to 255 do
        [%test_eq: t]
          (unsafe_of_int i)
          (of_int_exn    i)
          ~message:(string_of_int i)
      done

  end)

let%bench_module "int to char conversion" =
  (module struct

    let r = ref ' '

    let%bench "unsafe_of_int" =
      for i = 0 to 255 do
        r := unsafe_of_int i
      done

    let%bench "of_int_exn" =
      for i = 0 to 255 do
        r := of_int_exn i
      done

    let%bench "of_int" =
      for i = 0 to 255 do
        match of_int i with
        | Some t -> r := t
        | None   -> ()
      done

  end)

let escaped = Char.escaped

let lowercase = Char.lowercase

let uppercase = Char.uppercase

let is_lowercase t = 'a' <= t && t <= 'z'

let is_uppercase t = 'A' <= t && t <= 'Z'

let is_print t = ' ' <= t && t <= '~'

let is_whitespace = function
  | '\t'
  | '\n'
  | '\011' (* vertical tab *)
  | '\012' (* form feed *)
  | '\r'
  | ' '
    -> true
  | _
    -> false
;;

let%test _ = not (is_whitespace '\008') (* backspace *)
let%test _ =      is_whitespace '\009'  (* '\t': horizontal tab *)
let%test _ =      is_whitespace '\010'  (* '\n': line feed *)
let%test _ =      is_whitespace '\011'  (* '\v': vertical tab *)
let%test _ =      is_whitespace '\012'  (* '\f': form feed *)
let%test _ =      is_whitespace '\013'  (* '\r': carriage return *)
let%test _ = not (is_whitespace '\014') (* shift out *)
let%test _ =      is_whitespace '\032'  (* space *)

let is_digit t = '0' <= t && t <= '9'

let is_alpha t = is_lowercase t || is_uppercase t

let is_alphanum t = is_alpha t || is_digit t

let get_digit_unsafe t = to_int t - to_int '0'

let get_digit_exn t =
  if is_digit t
  then get_digit_unsafe t
  else failwithf "Char.get_digit_exn %C: not a digit" t ()
;;

let get_digit t = if is_digit t then Some (get_digit_unsafe t) else None

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

  let is_print_non_alphanum c =
    is_print c
    && not (is_alphanum c)

  let is_non_print c =
    not (is_print c)

  let gen_uppercase          = gen_matching_memoized is_uppercase
  let gen_lowercase          = gen_matching_memoized is_lowercase
  let gen_digit              = gen_matching_memoized is_digit
  let gen_whitespace         = gen_matching_memoized is_whitespace
  let gen_alpha              = gen_matching_memoized is_alpha
  let gen_alphanum           = gen_matching_memoized is_alphanum
  let gen_print_non_alphanum = gen_matching_memoized is_print_non_alphanum
  let gen_non_print          = gen_matching_memoized is_non_print

  let gen_print =
    Generator.weighted_union
      [ 5., gen_alphanum
      ; 1., gen_print_non_alphanum
      ]

  let gen =
    Generator.weighted_union
      [ 10., gen_print
      ;  1., gen_non_print
      ]

  let obs =
    Observer.enum 256
      ~f:to_int
      ~f_sexp:(fun () -> Atom "Char.to_int")

  let%bench_module "generators" =
    (module struct

      (* exported generators: *)
      let%bench "default"    = Quickcheck.iter gen            ~f:ignore
      let%bench "digit"      = Quickcheck.iter gen_digit      ~f:ignore
      let%bench "lowercase"  = Quickcheck.iter gen_lowercase  ~f:ignore
      let%bench "uppercase"  = Quickcheck.iter gen_uppercase  ~f:ignore
      let%bench "alpha"      = Quickcheck.iter gen_alpha      ~f:ignore
      let%bench "alphanum"   = Quickcheck.iter gen_alphanum   ~f:ignore
      let%bench "print"      = Quickcheck.iter gen_print      ~f:ignore
      let%bench "whitespace" = Quickcheck.iter gen_whitespace ~f:ignore

    end)

  let%test_module "generators" =
    (module struct

      let all = Set.of_array (Array.init 256 ~f:of_int_exn)

      let test gen ~f =
        (* repeat to make sure changing random seed doesn't affect the outcome *)
        for _ = 1 to 10 do
          [%test_result: Set.t]
            (Quickcheck.random_sequence gen
             |> Sequence.to_list
             |> Set.of_list)
            ~expect:(Set.filter all ~f)
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
