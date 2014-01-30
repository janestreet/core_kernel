open Int_replace_polymorphic_compare
include Binary_searchable_intf

module Make_gen (T : sig
                   type 'a elt
                   type 'a t
                   val get : 'a t -> int -> 'a elt
                   val length : _ t -> int
                 end) = struct

  let rec linear_search t ~lo ~hi ~compare v =
    if lo > hi
    then None
    else begin
      let c = compare v (T.get t lo) in
      if      c = 0 then Some lo
      else if c > 0 then linear_search t ~lo:(lo + 1) ~hi ~compare v
      else (* c < 0 *) None
    end
  ;;

  (** [find_range_near t ~max_size ~lo ~hi ~compare v] returns a range <= [max_size]
      between [lo] and [hi] guaranteed to contain [v] if [v] is in [t]. *)
  let find_range_near t ~max_size ~lo ~hi ~compare v =
    let rec loop ~lo ~hi =
      if hi - lo <= max_size
      then (lo, hi)
      else begin
        let mid = lo + ((hi - lo) / 2) in
        let c   = compare v (T.get t mid) in
        if      c = 0 then (mid, mid)
        else if c > 0 then loop ~lo:(mid + 1) ~hi
        else (* c < 0 *)   loop ~lo           ~hi:(mid - 1)
      end
    in
    loop ~lo ~hi
  ;;

  let binary_search ?pos ?len t ~compare v =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(T.length t)
    in
    let lo     = pos in
    let hi     = pos + len - 1 in
    let lo, hi = find_range_near t ~max_size:10 ~lo ~hi ~compare v in
    linear_search t ~lo ~hi ~compare v
  ;;
end

module Make (T : Indexable) =
  Make_gen (struct
    type 'a elt = T.elt
    type 'a t   = T.t
    include (T : Indexable with type elt := T.elt with type t := T.t)
  end)

module Make1 (T : Indexable1) =
  Make_gen (struct
    include T
    type 'a elt = 'a
  end)
