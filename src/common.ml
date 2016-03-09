open Sexplib.Conv

include Core_pervasives

(* See core_pervasives.ml for details *)
module Modified_INRIA_pervasives = struct
  include Pervasives
  external raise : exn -> 'a = "%reraise"
end

(* This is here just to assert that the interfaces match, so we'll notice when INRIA
   changes Pervasives. *)
include ((Core_pervasives           : module type of Modified_INRIA_pervasives) : sig end)
include ((Modified_INRIA_pervasives : module type of Core_pervasives)           : sig end)

include Perms.Export

include Never_returns

exception Finally = Exn.Finally

let protectx = Exn.protectx
let protect = Exn.protect

let (|!) = Fn.(|!)
let ident = Fn.id
let const = Fn.const
let (==>) a b = (not a) || b

let uw = function Some x -> x | None -> raise Not_found

let is_none = Option.is_none
let is_some = Option.is_some

let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let trd3 (_,_,z) = z

let ok_exn  = Or_error.ok_exn
let error   = Or_error.error
let error_s = Or_error.error_s

let failwiths = Error.failwiths
let failwithp = Error.failwithp
let raise_s   = Error.raise_s

include struct
  open Core_printf
  let failwithf = failwithf
  let invalid_argf = invalid_argf
end

(* module With_return only exists to avoid circular dependencies *)
include With_return

let phys_equal = Caml.(==)

let phys_same (type a) (type b) (a : a) (b : b) = phys_equal a (Obj.magic b : a)

let%test_module "phys_same" = (module struct

  let%test _ = phys_same 0 None
  let%test _ = phys_same 1 true

  let%test _ =
    let f () = "statically-allocated" in
    phys_same (f ()) (f ())
  ;;

  let%test _ =
    let a = (1, 2) in
    phys_same a a
  ;;

  type thing = Obscure : _ -> thing

  let same_thing (Obscure a) (Obscure b) = phys_same a b

  let%test _ =
    let a = (1, 2) in
    same_thing (Obscure a) (Obscure a)
  ;;
end)

let force = Lazy.force

let stage = Staged.stage
let unstage = Staged.unstage

exception Bug of string [@@deriving sexp]

exception C_malloc_exn of int * int (* errno, size *)
let () =
  Callback.register_exception "C_malloc_exn" (C_malloc_exn (0, 0));
