open Sexplib.Conv
module Sexp = Sexplib.Sexp
module List = ListLabels

include Comparable_intf

module Validate
    (T : sig type t [@@deriving compare, sexp] end) : Validate with type t := T.t =
struct

  module V = Validate
  open Maybe_bound

  let to_string t = Sexp.to_string (T.sexp_of_t t)

  let validate_bound ~min ~max t =
    V.bounded ~name:to_string ~lower:min ~upper:max ~compare:T.compare t
  ;;

  let validate_lbound ~min t = validate_bound ~min ~max:Unbounded t
  let validate_ubound ~max t = validate_bound ~max ~min:Unbounded t
end

module With_zero
    (T : sig
       type t [@@deriving compare, sexp]
       val zero : t
       include Validate with type t := t
     end) = struct
  open T

  (* Preallocate the interesting bounds to minimize allocation in the implementations of
     [validate_*]. *)
  let excl_zero = Maybe_bound.Excl zero
  let incl_zero = Maybe_bound.Incl zero

  let validate_positive     t = validate_lbound ~min:excl_zero t
  let validate_non_negative t = validate_lbound ~min:incl_zero t
  let validate_negative     t = validate_ubound ~max:excl_zero t
  let validate_non_positive t = validate_ubound ~max:incl_zero t
  let is_positive     t = compare t zero >  0
  let is_non_negative t = compare t zero >= 0
  let is_negative     t = compare t zero <  0
  let is_non_positive t = compare t zero <= 0
  let sign t = Sign0.of_int (compare t zero)
end

module Validate_with_zero
         (T : sig
            type t [@@deriving compare, sexp]
            val zero : t
          end) = struct
  module V = Validate (T)
  include V
  include With_zero (struct include T include V end)
end

module Map_and_set_binable (T : sig type t [@@deriving bin_io, compare, sexp] end) = struct
  module C = struct
    include T
    include Comparator.Make (T)
  end
  include C
  module Map = Core_map.Make_binable_using_comparator (C)
  module Set = Core_set.Make_binable_using_comparator (C)
end

module Poly (T : sig type t [@@deriving sexp] end) = struct
  module Replace_polymorphic_compare = struct
    type t = T.t [@@deriving sexp]
    include Polymorphic_compare
  end
  include Polymorphic_compare
  let ascending = compare
  let descending x y = compare y x
  let between t ~low ~high = low <= t && t <= high

  let clamp_unchecked t ~min ~max =
    if t < min then min else if t <= max then t else max

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max

  let clamp t ~min ~max =
    if min > max then
      Or_error.error "clamp requires [min <= max]"
        (`Min min, `Max max) [%sexp_of: [`Min of T.t] * [`Max of T.t]]
    else
      Ok (clamp_unchecked t ~min ~max)

  module C = struct
    include T
    include Comparator.Make (Replace_polymorphic_compare)
  end
  include C
  module Map = Core_map.Make_using_comparator (C)
  module Set = Core_set.Make_using_comparator (C)
  include Validate (struct type nonrec t = t [@@deriving compare, sexp] end)
end

module Make_common (T : sig
  type t [@@deriving compare, sexp]
end) = struct
  module Replace_polymorphic_compare = struct
    module Without_squelch = struct
      let compare = T.compare
      let (>) a b = compare a b > 0
      let (<) a b = compare a b < 0
      let (>=) a b = compare a b >= 0
      let (<=) a b = compare a b <= 0
      let (=) a b = compare a b = 0
      let (<>) a b = compare a b <> 0
      let equal = (=)
      let min t t' = if t <= t' then t else t'
      let max t t' = if t >= t' then t else t'
    end
    include Without_squelch
  end
  include Replace_polymorphic_compare.Without_squelch
  let ascending = compare
  let descending t t' = compare t' t
  let between t ~low ~high = low <= t && t <= high

  let clamp_unchecked t ~min ~max =
    if t < min then min else if t <= max then t else max

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max

  let clamp t ~min ~max =
    if min > max then
      Or_error.error "clamp requires [min <= max]"
        (`Min min, `Max max) [%sexp_of: [`Min of T.t] * [`Max of T.t]]
    else
      Ok (clamp_unchecked t ~min ~max)

  include Validate (T)
end

module Make_using_comparator (T : sig
    type t [@@deriving sexp]
    include Comparator.S with type t := t
  end) : S with type t := T.t and type comparator_witness = T.comparator_witness = struct
  include T
  include Make_common (struct
      include T
      let compare = comparator.compare
    end)
  module Map = Core_map.Make_using_comparator (T)
  module Set = Core_set.Make_using_comparator (T)
end

module Make (T : sig
  type t [@@deriving compare, sexp]
end) : S with type t := T.t = struct
  module C = struct
    include T
    include Comparator.Make (T)
  end
  include Make_using_comparator (C)
end

module Make_binable_using_comparator (T : sig
  type t [@@deriving bin_io, sexp]
  include Comparator.S with type t := t
end) = struct
  include T
  include Make_common (struct
      include T
      let compare = comparator.compare
    end)
  module Map = Core_map.Make_binable_using_comparator (T)
  module Set = Core_set.Make_binable_using_comparator (T)
end

module Make_binable (T : sig
  type t [@@deriving bin_io, compare, sexp]
end) = struct
  module C = struct
    include T
    include Comparator.Make (T)
  end
  include Make_binable_using_comparator (C)
end

module Inherit
  (C : sig type t [@@deriving compare] end)
  (T : sig
    type t [@@deriving sexp]
    val component : t -> C.t
  end) =
  Make (struct
    type t = T.t [@@deriving sexp]
    let compare t t' = C.compare (T.component t) (T.component t')
  end)

module Check_sexp_conversion (M : sig
  type t [@@deriving sexp_of]
  include S with type t := t
  val examples : t list
end) : sig end = struct
  open M

  let%test_unit _ =
    (* These tests all use single element sets and maps, and so do not depend on the
       order in which elements appear in sexps. *)
    List.iter examples ~f:(fun t ->
      let set = Set.of_list [ t ] in
      let set_sexp = Sexp.List [ sexp_of_t t ] in
      assert (Pervasives.(=) set_sexp ([%sexp_of: Set.t] set));
      assert (Set.equal set (Set.t_of_sexp set_sexp));
      let map = Map.of_alist_exn [ t, () ] in
      let map_sexp = Sexp.List [ Sexp.List [ sexp_of_t t; Sexp.List [] ]] in
      assert (Pervasives.(=) map_sexp ([%sexp_of: unit Map.t] map));
      assert (Map.equal (fun () () -> true)
                map (Map.t_of_sexp [%of_sexp: unit] map_sexp)))
  ;;
end

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let lexicographic cmps x y =
  let rec loop = function
    | cmp :: cmps -> let res = cmp x y in if res = 0 then loop cmps else res
    | [] -> 0
  in
  loop cmps
;;
