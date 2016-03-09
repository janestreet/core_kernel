module Sexp = Sexplib.Sexp
open Sexplib.Std
open Bin_prot.Std

include Sexp

exception Of_sexp_error = Sexplib.Conv.Of_sexp_error

module O = struct
  type sexp = Sexp.t = Atom of string | List of t list
end

module T : sig
  include Interfaces.Sexpable with type t := Sexp.t
  include Interfaces.Binable  with type t := Sexp.t
  val compare : t -> t -> int
end = struct
  type t = Sexp.t = Atom of string | List of t list [@@deriving bin_io, compare]

  let sexp_of_t t = t
  let t_of_sexp t = t
end

include T

module Sexp_maybe = struct

  type sexp = t [@@deriving bin_io, compare]             (* avoid recursive type *)

  (* to satisfy pa_compare *)
  module Error = struct
    include Error
    include Comparable.Poly (Error)
  end

  type 'a t = ('a, sexp * Error.t) Result.t [@@deriving bin_io, compare]

  let sexp_of_t sexp_of_a t =
    match t with
    | Result.Ok a -> sexp_of_a a
    | Result.Error (sexp, err) ->
      Sexp.List [
        Sexp.Atom "sexp_parse_error";
        sexp;
        Error.sexp_of_t err;
      ]

  let t_of_sexp a_of_sexp sexp =
    match sexp with
    | Sexp.List [ Sexp.Atom "sexp_parse_error"; sexp; _ ]
    | sexp ->
      try Result.Ok (a_of_sexp sexp)
      with exn -> Result.Error (sexp, Error.of_exn exn)

end

module With_text = struct
  open Result.Export

  type 'a t =
    { value: 'a
    ; text: string
    }
  [@@deriving bin_io]

  let sexp_of_t _ t = Sexp.Atom t.text

  let of_text value_of_sexp ?(filename="") text =
    match
      Or_error.try_with (fun () ->
        Sexp.of_string_conv (Core_string.strip text) value_of_sexp)
    with
    | Ok (`Result value) -> Ok { value; text }
    | Error _ as err -> err
    | Ok (`Error (exn, annotated)) ->
      Error (Error.of_exn (Sexp.Annotated.get_conv_exn annotated ~file:filename ~exc:exn))

  let t_of_sexp a_of_sexp sexp =
    match sexp with
    | List _ ->
      Sexplib.Conv.of_sexp_error
        "With_text.t should be stored as an atom, but instead a list was found." sexp
    | Atom text ->
      of_text a_of_sexp text |> Or_error.ok_exn

  let text  t = t.text
  let value t = t.value

  let of_value sexp_of_value value =
    let text = sexp_of_value value |> Sexp.to_string_hum in
    { value; text }

  let%test_module _ = (module struct
    let sexp_of_il = sexp_of_list sexp_of_int
    let il_of_sexp = list_of_sexp int_of_sexp

    let il_of_text text = Or_error.ok_exn (of_text il_of_sexp text)
    let il_of_value il  = of_value sexp_of_il il

    let t = il_of_value [3;4]
    let%test _ = t.text = "(3 4)"
    let t' = il_of_text (text t)
    let%test _ = t'.value = [3;4]
    let%test _ = sexp_of_t sexp_of_il t = Atom "(3 4)"
    let%test _ = (t_of_sexp il_of_sexp (Atom "(3 4)")).value = [3;4]

    let%test _ = [8;9] = (il_of_text ";this is a comment\n (8; foo\n 9)   \n ").value

    let check_error f input ~expected =
      let normalize str = try Sexp.to_string (Sexp.of_string str) with _ -> str in
      let expected = normalize expected in
      try
        ignore (f input);
        failwith
          (Printf.sprintf "%s expected to cause an exception, \
                           but got converted successfully." input)
      with e ->
        let error = normalize (Printexc.to_string e) in
        if error <> expected then
          failwith (Printf.sprintf "%s generated error %s, expected %s"
                      input error expected)

    let expected =
      "(Sexplib.Conv.Of_sexp_error(
        Sexplib.Sexp.Annotated.Conv_exn
        :1:5(Failure\"int_of_sexp: (Failure int_of_string)\"))bla)"

    let%test_unit _ =
      check_error il_of_text
        "(1 2 bla)" ~expected

    let%test_unit _ =
      check_error (fun s -> t_of_sexp il_of_sexp (Sexp.of_string s))
        "\"(1 2 bla)\"" ~expected
  end)
end


let of_int_style = Int_conversions.sexp_of_int_style

type 'a no_raise = 'a [@@deriving bin_io, sexp]

let sexp_of_no_raise sexp_of_a a =
  try sexp_of_a a
  with exn ->
    try List [ Atom "failure building sexp"; sexp_of_exn exn ]
    with _ -> Atom "could not build sexp for exn raised when building sexp for value"
;;

include Comparable.Make (struct
  type t = Sexp.t
  include T
end)

let of_sexp_allow_extra_fields of_sexp sexp =
  let r = Sexplib.Conv.record_check_extra_fields in
  let prev = !r in
  Exn.protect ~finally:(fun () -> r := prev)
    ~f:(fun () -> r := false; of_sexp sexp)

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Monad_infix

  let gen =
    Generator.recursive (fun t_gen ->
      Generator.variant2
        Core_string.gen
        (Core_list.gen t_gen)
      >>| function
      | `A atom -> Sexp.Atom atom
      | `B list -> Sexp.List list)

  let obs =
    Observer.recursive (fun t_obs ->
      Observer.unmap
        (Observer.variant2
           Core_string.obs
           (Core_list.obs t_obs))
        ~f:(function
          | Sexp.Atom atom -> `A atom
          | Sexp.List list -> `B list)
        ~f_sexp:(fun () -> Sexp.Atom "variant_of_sexp"))

  let shrinker =
    let open Sequence.Monad_infix in
    Shrinker.recursive (fun shrinker ->
      Shrinker.create (function
        | Sexp.Atom _    -> Sequence.empty
        | Sexp.List list ->
          let shrink_list =
            Shrinker.shrink (Core_list.shrinker shrinker) list
            >>| fun l -> Sexp.List l
          in
          let shrink_tree = Sequence.of_list list in
          Sequence.interleave (Sequence.of_list [ shrink_list; shrink_tree ])))

end

let gen      = For_quickcheck.gen
let obs      = For_quickcheck.obs
let shrinker = For_quickcheck.shrinker
