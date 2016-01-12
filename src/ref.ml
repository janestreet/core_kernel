module T = struct
  (* In the definition of [t], we do not have [[@@deriving bin_io, compare, sexp]] because
     in general, syntax extensions tend to use the implementation when available rather
     than using the alias.  Here that would lead to use the record representation [ {
     mutable contents : 'a } ] which would result in different (and unwanted)
     behavior.  *)
  type 'a t = 'a ref = { mutable contents : 'a }

  include (struct
    open Typerep_lib.Std
    open Sexplib.Conv
    open Bin_prot.Std
    type 'a t = 'a ref [@@deriving bin_io, compare, sexp, typerep]
  end : sig
    type 'a t = 'a ref [@@deriving bin_io, compare, sexp, typerep]
  end with type 'a t := 'a t)

  let create x = ref x

  let (!) = Pervasives.(!)
  let (:=) = Pervasives.(:=)

  let swap t1 t2 =
    let tmp = !t1 in
    t1 := !t2;
    t2 := tmp

  let replace t f = t := f !t

  (* container functions below *)
  let length _ = 1

  let is_empty _ = false

  let iter t ~f = f !t

  let fold t ~init ~f = f init !t

  let count t ~f = if f !t then 1 else 0
  let sum _ t ~f = f !t

  let exists t ~f = f !t

  let for_all t ~f = f !t

  let mem ?(equal = (=)) t a = equal a !t

  let find t ~f = let a = !t in if f a then Some a else None

  let find_map t ~f = f !t

  let to_list t = [ !t ]

  let to_array t = [| !t |]

  let min_elt t ~cmp:_ = Some !t
  let max_elt t ~cmp:_ = Some !t

  let set_temporarily t a ~f =
    let restore_to = !t in
    t := a;
    Exn.protect ~f ~finally:(fun () -> t := restore_to);
  ;;
end

include T

module Permissioned = struct
  include (T : (module type of T) with type 'a t := 'a ref)

  type ('a, -'perms) t = 'a T.t [@@deriving bin_io, sexp]

  let read_only = Fn.id
  let of_ref    = Fn.id
  let to_ref    = Fn.id

  let set = (:=)
  let get = (!)
end
