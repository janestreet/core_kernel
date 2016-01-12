open Std_internal

open Validated_intf

module type Raw = Raw

type 'a t = 'a

module type Validated         = Validated         with type 'a validated := 'a t
module type Validated_binable = Validated_binable with type 'a validated := 'a t

let raw t = t

module Make (Raw : Raw) = struct

  type t = Raw.t [@@deriving sexp_of]

  let validation_failed t error =
    Error.create "validation failed" (t, error, Raw.here)
      [%sexp_of: Raw.t * Error.t * Source_code_position.t]
  ;;

  let create_exn t =
    match Validate.result (Raw.validate t) with
    | Ok () -> t
    | Error error -> Error.raise (validation_failed t error)
  ;;

  let create t =
    match Validate.result (Raw.validate t) with
    | Ok () -> Ok t
    | Error error -> Error (validation_failed t error)
  ;;

  let t_of_sexp sexp = create_exn (Raw.t_of_sexp sexp)

  let raw t = t

end

module Make_binable (Raw : Raw_binable) = struct

  include Make (Raw)

  include Binable.Of_binable (Raw) (struct
    type t = Raw.t
    let of_binable raw =
      if Raw.validate_binio_deserialization
      then create_exn raw
      else raw
    ;;
    let to_binable = Fn.id
  end)
end

let%test_module _ = (module struct

  module Positive_int = struct
    type t = int [@@deriving bin_io, sexp]
    let validate t =
      if t > 0
      then Validate.pass
      else Validate.fail "must be positive"
    ;;
  end

  let does_raise = Exn.does_raise

  (* The [: Validated] is to remind us to add a unit test whenever the [Validated]
     interface changes. *)
  module M : Validated with type raw := int = struct

    module M = Make (struct
      let here = [%here]
      include Positive_int
    end)

    open M

    type nonrec t = t

    let t_of_sexp = t_of_sexp
    let sexp_of_t = sexp_of_t

    let%test_unit _ = assert (does_raise (fun () -> t_of_sexp ([%sexp_of: int] 0)))

    let%test_unit _ =
      let sexp = [%sexp_of: int] 13 in
      assert (sexp_of_t (t_of_sexp sexp) = sexp)
    ;;

    let create     = create
    let create_exn = create_exn
    let raw        = raw

    let%test_unit _ = assert (does_raise (fun () -> create_exn 0))

    let%test_unit _ =
      match create 0 with
      | Error _ -> ()
      | Ok _ -> assert false
    ;;

    let%test_unit _ =
      let n = 13 in
      let t = create_exn n in
      assert (raw t = n)
    ;;

    let%test_unit _ =
      let n = 13 in
      match create n with
      | Error _ -> assert false
      | Ok t -> assert ((t :> int) = n)
    ;;
  end

  module M1 = Make_binable (struct
    let here = [%here]
    let validate_binio_deserialization = true
    include Positive_int
  end)

  module M2 = Make_binable (struct
    let here = [%here]
    let validate_binio_deserialization = false
    include Positive_int
  end)

  let int = 0
  let string = Binable.to_string (module Int) int
  let%test _ = does_raise (fun () -> Binable.of_string (module M1) string)
  let%test _ = (Binable.of_string (module M2) string) = int

  let int = 1
  let string = Binable.to_string (module Int) int
  let%test _ = Binable.of_string (module M1) string = int
  let%test _ = Binable.of_string (module M2) string = int

end)
