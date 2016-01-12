module Sexp = Sexplib.Sexp
open Sexplib.Std

let failwiths = Error.failwiths

type ('a, 'b) t = ('a, 'b) Typerep_lib.Std.Type_equal.t = T : ('a, 'a) t
type ('a, 'b) equal = ('a, 'b) t

let refl = T

let sym (type a) (type b) (T : (a, b) t) = (T : (b, a) t)

let trans (type a) (type b) (type c) (T : (a, b) t) (T : (b, c) t) = (T : (a, c) t)

let conv (type a) (type b) (T : (a, b) t) (a : a) = (a : b)

module Lift (X : sig type 'a t end) = struct
  let lift (type a) (type b) (T : (a, b) t) = (T : (a X.t, b X.t) t)
end

module Lift2 (X : sig type ('a1, 'a2) t end) = struct
  let lift (type a1) (type b1) (type a2) (type b2) (T : (a1, b1) t) (T : (a2, b2) t) =
    (T : ((a1, a2) X.t, (b1, b2) X.t) t)
  ;;
end

module Lift3 (X : sig type ('a1, 'a2, 'a3) t end) = struct
  let lift (type a1) (type b1) (type a2) (type b2) (type a3) (type b3)
        (T : (a1, b1) t) (T : (a2, b2) t) (T : (a3, b3) t) =
    (T : ((a1, a2, a3) X.t, (b1, b2, b3) X.t) t)
  ;;
end

let detuple2 (type a1) (type a2) (type b1) (type b2)
    (T : (a1 * a2, b1 * b2) t) : (a1, b1) t * (a2, b2) t =
  T, T
;;

let tuple2 (type a1) (type a2) (type b1) (type b2)
    (T : (a1, b1) t) (T : (a2, b2) t) : (a1 * a2, b1 * b2) t =
  T
;;

module type Injective = sig
  type 'a t
  val strip : ('a t, 'b t) equal -> ('a, 'b) equal
end

module type Injective2 = sig
  type ('a1, 'a2)  t
  val strip : (('a1, 'a2) t, ('b1, 'b2) t) equal -> ('a1, 'b1) equal * ('a2, 'b2) equal
end

module Composition_preserves_injectivity (M1 : Injective) (M2 : Injective) = struct
  type 'a t = 'a M1.t M2.t
  let strip e = M1.strip (M2.strip e)
end

module Id = struct
  module Uid = Core_int

  module Witness = struct
    module Key = struct
      type _ t = ..

      let sexp_of_t _sexp_of_a t =
        (`type_witness (Obj.extension_id t)) |> [%sexp_of: [ `type_witness of int ]]
      ;;
    end

    module type S = sig
      type t
      type _ Key.t += Key : t Key.t
    end

    type 'a t = (module S with type t = 'a)

    let sexp_of_t (type a) sexp_of_a (module M : S with type t = a) =
      M.Key |> [%sexp_of: a Key.t]
    ;;

    let create (type t) () =
      let module M = struct
        type nonrec t = t
        type _ Key.t += Key : t Key.t
      end in
      (module M : S with type t = t)
    ;;

    let uid (type a) (module M : S with type t = a) = Obj.extension_id M.Key

    (* We want a constant allocated once that [same] can return whenever it gets the same
       witnesses.  If we write the constant inside the body of [same], the native-code
       compiler will do the right thing and lift it out.  But for clarity and robustness,
       we do it ourselves. *)
    let some_t = Some T

    let same (type a) (type b) (a : a t) (b : b t) : (a, b) equal option =
      let module A = (val a : S with type t = a) in
      let module B = (val b : S with type t = b) in
      match A.Key with
      | B.Key -> some_t
      | _     -> None
    ;;
  end


  type 'a t =
    { witness : 'a Witness.t
    ; name    : string
    ; to_sexp : 'a -> Sexp.t
    }
  [@@deriving fields, sexp_of]

  let create ~name to_sexp =
    { witness = Witness.create ()
    ; name
    ; to_sexp
    }
  ;;

  let uid t = Witness.uid t.witness

  let hash t = uid t

  let same_witness t1 t2 = Witness.same t1.witness t2.witness

  let same t1 t2 = Option.is_some (same_witness t1 t2)

  let same_witness_exn t1 t2 =
    match same_witness t1 t2 with
    | Some w -> w
    | None -> failwiths "Type_equal.Id.same_witness_exn got different ids" (t1, t2)
                [%sexp_of: _ t * _ t]
  ;;

  let%test_module _ = (module struct
    let t1 = create ~name:"t1" [%sexp_of: _]
    let t2 = create ~name:"t2" [%sexp_of: _]

    let%test _ =      same t1 t1
    let%test _ = not (same t1 t2)

    let%test _ = Option.is_some (same_witness t1 t1)
    let%test _ = Option.is_none (same_witness t1 t2)

    let%test_unit _ = ignore (same_witness_exn t1 t1 : (_, _) equal)
    let%test _ = Result.is_error (Result.try_with (fun () -> same_witness_exn t1 t2))
  end)
end

(* This test shows that we need [conv] even though [Type_equal.T] is exposed. *)
let%test_module _ = (module struct
  let id = Id.create ~name:"int" [%sexp_of: int]

  module A : sig
    type t
    val id : t Id.t
  end = struct
    type t = int
    let id = id
  end

  module B : sig
    type t
    val id : t Id.t
  end = struct
    type t = int
    let id = id
  end

  let _a_to_b (a : A.t) =
    let eq = Id.same_witness_exn A.id B.id in
    (conv eq a : B.t)
  ;;

  (* the following is rejected by the compiler *)
  (* let _a_to_b (a : A.t) =
   *   let T = Id.same_witness_exn A.id B.id in
   *   (a : B.t)
   *)

  module C = struct
    type 'a t
  end

  module Liftc = Lift (C)

  let _ac_to_bc (ac : A.t C.t) =
    let eq = Liftc.lift (Id.same_witness_exn A.id B.id) in
    (conv eq ac : B.t C.t)
  ;;
end)
