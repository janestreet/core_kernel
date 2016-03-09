module Binable = Binable0
module Map = Core_map
module Sexp = Sexplib.Sexp

let failwiths = Error.failwiths

module type S = sig
  type t [@@deriving bin_io, sexp]
  include Stringable.S         with type t := t
  include Comparable.S_binable with type t := t
  include Hashable  .S_binable with type t := t
  include Pretty_printer.S     with type t := t
end

module Make (T : sig
  type t [@@deriving bin_io, compare, sexp]
  include Stringable.S with type t := t
  val hash : t -> int
  val module_name : string
end) = struct
  include T
  include Comparable.Make_binable (T)
  include Hashable  .Make_binable (T)
  include Pretty_printer.Register (T)
end

module Make_using_comparator (T : sig
  type t [@@deriving bin_io, compare, sexp]
  include Comparator.S with type t := t
  include Stringable.S with type t := t
  val hash : t -> int
  val module_name : string
end) = struct
  include T
  include Comparable.Make_binable_using_comparator (T)
  include Hashable  .Make_binable (T)
  include Pretty_printer.Register (T)
end

(* The unit test below checks that for a call to [Identifiable.Make], the functions in the
   resulting module call the functions in the argument module the correct number of
   times. *)
let%test_module _ = (module struct
  open Sexplib.Conv

  module Counter = struct
    type t =
    | Compare
    | Hash
    | Of_string
    | Sexp_of_t
    | T_of_sexp
    | To_string
    [@@deriving compare, sexp]
  end

  open Counter

  module Counts = struct
    module Map = Map.Make (Counter)

    type t = int Map.t ref [@@deriving sexp_of]

    let actual = ref Map.empty
    let expected = ref Map.empty

    let incr ?(by = 1) t counter =
      t := Map.update !t counter ~f:(function None -> by | Some i -> i + by)
    ;;

    let check location =
      if not (Map.equal (=) !actual !expected) then
        failwiths "mismatch" (location, `actual actual, `expected expected)
          [%sexp_of: Source_code_position0.t * [ `actual of t ] * [ `expected of t ]]
    ;;
  end

  module T = struct

    let module_name = "Core.Std.Identifiable.T"

    type t = A | B [@@deriving bin_io, compare, sexp]

    let hash (t : t) = Hashtbl.hash t

    include Sexpable.To_stringable (struct type nonrec t = t [@@deriving sexp] end)

    let incr ?by counter = Counts.incr Counts.actual counter ?by

    let compare t1 t2 = incr Compare; compare t1 t2
    let hash t = incr Hash; hash t
    let sexp_of_t t = incr Sexp_of_t; sexp_of_t t
    let t_of_sexp t = incr T_of_sexp; t_of_sexp t
    let of_string t = incr Of_string; of_string t
    let to_string t = incr To_string; to_string t
  end

  module Id = Make (T)

  let poly_equal = (=)
  let int_equal (i1 : int) i2 = poly_equal i1 i2

  let%test_unit _ =
    let open T in
    let open Id in
    let check = Counts.check in
    let incr ?by counter = Counts.incr Counts.expected counter ?by in
    check [%here];
    ignore (to_string A : string);
    incr To_string;
    check [%here];
    ignore (of_string "A" : t);
    incr Of_string;
    check [%here];
    ignore (t_of_sexp (Sexp.of_string "A") : t);
    incr T_of_sexp;
    check [%here];
    ignore (sexp_of_t A : Sexp.t);
    incr Sexp_of_t;
    check [%here];
    assert (int_equal (compare A A) 0);
    incr Compare;
    check [%here];
    assert (int_equal (compare A B) (-1));
    incr Compare;
    check [%here];
    assert (int_equal (compare B A) 1);
    incr Compare;
    check [%here];
    ignore (not (int_equal (hash A) (hash B)));
    incr Hash ~by:2;
    check [%here];
    let bigstring = Binable.to_bigstring (module T) A in
    check [%here];
    assert (poly_equal A (Binable.of_bigstring (module T) bigstring));
    check [%here]
  ;;
end)
