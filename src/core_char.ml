open Typerep_lib.Std
open Sexplib.Std
open Bin_prot.Std
open Hash.Builtin
module Char  = Caml.Char
module List  = Caml.ListLabels
module Array = Caml.ArrayLabels

let failwithf = Core_printf.failwithf

module T = struct
  type t = char [@@deriving hash, bin_io, sexp, typerep]

  let compare = Char.compare

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

let escaped = Char.escaped

let lowercase = Char.lowercase_ascii

let uppercase = Char.uppercase_ascii

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_print = function
  | ' ' .. '~' -> true
  | _ -> false

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

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

(* Writing these out, instead of calling [is_alpha] and [is_digit], reduces
   runtime by approx. 30% *)
let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

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
