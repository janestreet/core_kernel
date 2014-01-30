open Sexplib.Conv

let seek_out _ _ = `Deprecated_use_out_channel
let pos_out _ = `Deprecated_use_out_channel
let out_channel_length _ = `Deprecated_use_out_channel
let seek_in _ _ = `Deprecated_use_in_channel
let pos_in _ = `Deprecated_use_in_channel
let in_channel_length _ = `Deprecated_use_in_channel
let modf _ = `Deprecated_use_float_modf
let truncate _ = `Deprecated_use_float_iround_towards_zero

let ( & ) _ _ = `Deprecated_use_two_ampersands

(* let ( or ) =  `Deprecated_use_pipe_pipe *)

let max_int =  `Deprecated_use_int_module
let min_int =  `Deprecated_use_int_module

let ceil _            = `Deprecated_use__Float__round_up
let floor _           = `Deprecated_use__Float__round_down
let abs_float _       = `Deprecated_use_float_module
let mod_float _       = `Deprecated_use_float_module
let frexp _ _         = `Deprecated_use_float_module
let ldexp _ _         = `Deprecated_use_float_module
let float_of_int _    = `Deprecated_use_float_module
let max_float         = `Deprecated_use_float_module
let min_float         = `Deprecated_use_float_module
let epsilon_float     = `Deprecated_use_float_module
let classify_float _  = `Deprecated_use_float_module
let string_of_float _ = `Deprecated_use_float_module
let float_of_string _ = `Deprecated_use_float_module
let infinity          = `Deprecated_use_float_module
let neg_infinity      = `Deprecated_use_float_module
let nan               = `Deprecated_use_float_module
let int_of_float _    = `Deprecated_use_float_module

type fpclass =        [`Deprecated_use_float_module ]

let close_in _ = `Deprecated_use_in_channel
let close_out _ = `Deprecated_use_out_channel

type read_only with bin_io, compare
include (struct
  type immutable  = read_only with bin_io, compare
  type read_write = read_only with bin_io, compare
end : sig
  type immutable  = private read_only with bin_io, compare
  type read_write = private read_only with bin_io, compare
end)

(* These are to work around a bug in pa_sexp where sexp_of_immutable would assert false
   rather than give a clear error message. *)
let sexp_of_immutable _ = failwith "attempt to convert abstract type immutable"
let immutable_of_sexp = sexp_of_immutable
let sexp_of_read_only _ = failwith "attempt to convert abstract type read_only"
let read_only_of_sexp = sexp_of_read_only
let sexp_of_read_write _ = failwith "attempt to convert abstract type read_write"
let read_write_of_sexp = sexp_of_read_write

include Never_returns

exception Finally = Exn.Finally

let protectx = Exn.protectx
let protect = Exn.protect

let (|!) = Fn.(|!)
let (|>) = Fn.(|>)
let ident = Fn.id
let const = Fn.const
let (==>) a b = (not a) || b

let uw = function Some x -> x | None -> raise Not_found

let is_none = Option.is_none
let is_some = Option.is_some

let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let trd3 (_,_,z) = z

let does_raise (type a) (f : unit -> a) =
  try
    ignore (f () : a);
    false
  with _ ->
    true
;;

TEST = not (does_raise Fn.ignore)
TEST = does_raise (fun () -> failwith "foo")

let ok_exn = Or_error.ok_exn
let error = Or_error.error

let failwiths = Error.failwiths
let failwithp = Error.failwithp

include struct
  open Core_printf
  let failwithf = failwithf
  let invalid_argf = invalid_argf
end

(* module With_return only exists to avoid circular dependencies *)
include With_return

let phys_equal = Caml.(==)
let (==) _ _ = `Consider_using_phys_equal
let (!=) _ _ = `Consider_using_phys_equal

let phys_same (type a) (type b) (a : a) (b : b) = phys_equal a (Obj.magic b : a)

TEST_MODULE "phys_same" = struct

  TEST = phys_same 0 None
  TEST = phys_same 1 true

  TEST =
    let f () = "statically-allocated" in
    phys_same (f ()) (f ())
  ;;

  TEST =
    let a = (1, 2) in
    let b = (1, 2) in
    phys_same a a && not (phys_same a b)
  ;;

  type thing = Obscure : _ -> thing

  let same_thing (Obscure a) (Obscure b) = phys_same a b

  TEST =
    let a = (1, 2) in
    let b = (1, 2) in
    same_thing (Obscure a) (Obscure a) && not (same_thing (Obscure a) (Obscure b))
  ;;
end

let force = Lazy.force

let stage = Staged.stage
let unstage = Staged.unstage

let unimplemented = Or_error.unimplemented

exception Bug of string with sexp

exception C_malloc_exn of int * int (* errno, size *)
let () =
  Callback.register_exception "C_malloc_exn" (C_malloc_exn (0, 0));
