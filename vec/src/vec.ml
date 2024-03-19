open! Core

module With_integer_index = struct
  (* Kernel hides away Obj-handling. *)
  module Kernel : sig
    type 'a t

    val length : _ t -> int
    val capacity : _ t -> int
    val create : ?initial_capacity:int -> unit -> _ t
    val unsafe_create_uninitialized : len:int -> 'a t
    val init : int -> f:(int -> 'a) -> 'a t
    val unsafe_get : 'a t -> int -> 'a
    val unsafe_set : 'a t -> int -> 'a -> unit

    val unsafe_blit
      :  src:'a t
      -> src_pos:int
      -> dst:'a t
      -> dst_pos:int
      -> len:int
      -> unit

    val invariant : 'a Invariant.t -> 'a t Invariant.t
    val max_index : _ t -> int
    val grow_capacity_once : _ t -> unit
    val grow_capacity_to_at_least : _ t -> capacity:int -> unit
    val unsafe_clear_pointer_at : _ t -> int -> unit
    val set_length : _ t -> int -> unit
    val copy : 'a t -> 'a t
    val sort : ?pos:int -> ?len:int -> 'a t -> compare:('a -> 'a -> int) -> unit

    module Expert : sig
      val unsafe_inner : 'a t -> Obj.t Uniform_array.t
    end

    module With_structure_details : sig
      type nonrec 'a t = 'a t [@@deriving sexp_of]
    end
  end = struct
    type 'a t =
      { mutable arr : Obj.t Uniform_array.t
      ; mutable length : int
      ; mutable capacity : int
          (** Invariant: [capacity = Uniform_array.length arr].
          We maintain it here to eliminate an indirection when accessing long arrays. *)
      }
    [@@deriving fields ~getters ~setters]

    let length t = t.length

    let check_capacity capacity =
      if capacity < 0 then invalid_argf "Vec: negative capacity %d" capacity ()
    ;;

    (* [initial_capacity] is mostly arbitrary, but it does make our array take one header
       word + 7 data words = 8 words * 8 bytes = 64 bytes = one cacheline by default. (Of
       course, there's no alignment guarantee.) *)
    let create ?(initial_capacity = 7) () =
      check_capacity initial_capacity;
      { arr = Uniform_array.unsafe_create_uninitialized ~len:initial_capacity
      ; length = 0
      ; capacity = initial_capacity
      }
    ;;

    let unsafe_create_uninitialized ~len:n =
      check_capacity n;
      { arr = Uniform_array.unsafe_create_uninitialized ~len:n; length = n; capacity = n }
    ;;

    let init n ~f =
      check_capacity n;
      { arr = Uniform_array.init n ~f:(fun i -> f i |> Obj.magic)
      ; length = n
      ; capacity = n
      }
    ;;

    let copy t =
      { arr = Uniform_array.copy t.arr; length = t.length; capacity = t.capacity }
    ;;

    let[@inline always] unsafe_get (type a) (t : a t) i : a =
      Uniform_array.unsafe_get t.arr i |> Obj.magic
    ;;

    let[@inline always] unsafe_set (type a) (t : a t) i (element : a) =
      Uniform_array.unsafe_set t.arr i (Obj.repr element)
    ;;

    let[@inline always] unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
      Uniform_array.unsafe_blit ~src:src.arr ~src_pos ~dst:dst.arr ~dst_pos ~len
    ;;

    module With_structure_details = struct
      type nonrec 'a t = 'a t

      let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) (t : a t) =
        let { arr; length; capacity } = t in
        let elements =
          Uniform_array.init (Uniform_array.length arr) ~f:(fun i ->
            let element = Uniform_array.get arr i in
            (* Only the first [length] elements can safely be given to [sexp_of_a]. *)
            if i < length
            then element |> Obj.magic |> sexp_of_a
            else (
              let imm : int = Obj.magic element in
              Sexp.Atom (sprintf "_%d" imm)))
        in
        [%sexp { elements : Sexp.t Uniform_array.t; length : int; capacity : int }]
      ;;
    end

    let invariant (type a) (a_inv : a Invariant.t) (t : a t) =
      Invariant.invariant [%here] t [%sexp_of: _ With_structure_details.t] (fun () ->
        let { capacity; length; arr } = t in
        if capacity <> Uniform_array.length t.arr
        then
          raise_s
            [%message
              "capacity should equal Option_array length"
                (capacity : int)
                (Uniform_array.length t.arr : int)];
        if capacity < 0 then raise_s [%message "negative capacity" (capacity : int)];
        if length > capacity
        then
          raise_s
            [%message
              "length shouldn't be more than capacity" (length : int) (capacity : int)];
        for pos = 0 to length - 1 do
          a_inv (Uniform_array.get arr pos |> Obj.magic)
        done;
        for pos = length to capacity - 1 do
          assert (Uniform_array.get arr pos |> Obj.is_int)
        done)
    ;;

    let[@inline always] max_index t = t.length - 1

    let grow_capacity_to_exactly t ~capacity =
      let arr = Uniform_array.unsafe_create_uninitialized ~len:capacity in
      for i = 0 to max_index t do
        Uniform_array.unsafe_set arr i (Uniform_array.unsafe_get t.arr i)
      done;
      t.arr <- arr;
      t.capacity <- capacity
    ;;

    let growth_factor = 2

    let grow_capacity_once t =
      grow_capacity_to_exactly t ~capacity:(Int.max 1 t.capacity * growth_factor)
    ;;

    let grow_capacity_to_at_least t ~capacity:target_capacity =
      assert (growth_factor = 2);
      if t.capacity < target_capacity
      then
        grow_capacity_to_exactly t ~capacity:(Int.ceil_pow2 (Int.max 1 target_capacity))
    ;;

    let[@inline always] unsafe_clear_pointer_at t pos =
      Uniform_array.unsafe_clear_if_pointer t.arr pos
    ;;

    let sort (type a) ?pos ?len t ~(compare : a -> a -> int) =
      let compare : Obj.t -> Obj.t -> int = Obj.magic compare in
      (* [Uniform_array] checks this but has an overestimate of our length. *)
      let pos, len =
        Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
      in
      Uniform_array.sort ~pos ~len t.arr ~compare
    ;;

    module Expert = struct
      let[@inline always] unsafe_inner t = t.arr
    end
  end

  include Kernel

  let is_sorted t ~compare =
    (* This is a copy-paste from [Array.is_sorted]. *)
    let i = ref (length t - 1) in
    let result = ref true in
    while !i > 0 && !result do
      let elt_i = unsafe_get t !i in
      let elt_i_minus_1 = unsafe_get t (!i - 1) in
      if compare elt_i_minus_1 elt_i > 0 then result := false;
      decr i
    done;
    !result
  ;;

  let next_free_index = length

  let[@cold] raise__bad_index t i ~op =
    raise_s
      [%message
        "tried to access vec out of bounds"
          (t : _ With_structure_details.t)
          (i : int)
          (op : string)]
  ;;

  let[@inline always] check_index t i ~op =
    if i < 0 || i >= length t then raise__bad_index t i ~op
  ;;

  let get t i =
    check_index t i ~op:"get";
    unsafe_get t i
  ;;

  let maybe_get t i = if i < 0 || i >= length t then None else Some (unsafe_get t i)

  let maybe_get_local t i =
    if i < 0 || i >= length t then None else Some { Gel.g = unsafe_get t i }
  ;;

  let set t i element =
    check_index t i ~op:"set";
    unsafe_set t i element
  ;;

  let[@inline always] push_back__we_know_we_have_space t element =
    let length = length t in
    unsafe_set t length element;
    set_length t (length + 1)
  ;;

  let push_back_index t element =
    let length = length t in
    if length = capacity t then grow_capacity_once t;
    push_back__we_know_we_have_space t element;
    length
  ;;

  let[@inline always] push_back t element =
    let (_ : int) = push_back_index t element in
    ()
  ;;

  let remove_exn t i =
    if i < 0 || i >= length t then raise__bad_index t i ~op:"remove_exn";
    let new_length = length t - 1 in
    (* As per the ocaml stdlib documentation, blitting with src and dst
       overlapping is safe.
       https://github.com/ocaml-flambda/flambda-backend/blob/main/ocaml/stdlib/array.mli#L143
    *)
    unsafe_blit ~src:t ~src_pos:(i + 1) ~dst:t ~dst_pos:i ~len:(length t - i - 1);
    set_length t new_length
  ;;

  let[@inline always] unsafe_peek_back_exn t = unsafe_get t (max_index t)

  let peek_back_exn t =
    let length = length t in
    if length <= 0 then raise__bad_index t length ~op:"peek_back";
    unsafe_peek_back_exn t
  ;;

  let peek_back t = if length t <= 0 then None else Some (unsafe_peek_back_exn t)

  let[@inline always] pop_back_unit_exn t =
    let pos = max_index t in
    (* Don't leak the value. *)
    unsafe_clear_pointer_at t pos;
    set_length t pos
  ;;

  let pop_back_exn t =
    let e = peek_back_exn t in
    pop_back_unit_exn t;
    e
  ;;

  let[@inline never] grow_to_unchecked t ~len ~default =
    grow_capacity_to_at_least t ~capacity:len;
    for i = length t to len - 1 do
      unsafe_set t i default
    done;
    set_length t len
  ;;

  let[@inline always] grow_to t ~len ~default =
    if len > length t then grow_to_unchecked t ~len ~default
  ;;

  let grow_to_include t idx ~default = grow_to t ~len:(idx + 1) ~default

  let grow_to' t ~len ~default =
    if len > length t
    then (
      grow_capacity_to_at_least t ~capacity:len;
      for i = length t to len - 1 do
        unsafe_set t i (default i)
      done;
      set_length t len)
  ;;

  let grow_to_include' t idx ~default = grow_to' t ~len:(idx + 1) ~default

  let shrink_to t ~len =
    if len < 0
    then raise__bad_index t len ~op:"shrink_to"
    else if len < length t
    then (
      for i = len to max_index t do
        unsafe_clear_pointer_at t i
      done;
      set_length t len)
  ;;

  let shrink_to_imm (t : 'a t) (_ : 'a Type_immediacy.Always.t) ~len =
    if len < 0
    then raise__bad_index t len ~op:"shrink_to_imm"
    else if len < length t
    then set_length t len
  ;;

  let iteri t ~f =
    for i = 0 to max_index t do
      f i (unsafe_get t i)
    done
  ;;

  let iter t ~f =
    for i = 0 to max_index t do
      f (unsafe_get t i)
    done
  ;;

  let to_list t =
    let result = ref [] in
    for i = max_index t downto 0 do
      result := unsafe_get t i :: !result
    done;
    !result
  ;;

  let to_local_list t =
    let rec aux t i acc = if i < 0 then acc else aux t (i - 1) (unsafe_get t i :: acc) in
    aux t (max_index t) []
  ;;

  let to_alist t =
    let result = ref [] in
    for i = max_index t downto 0 do
      result := (i, unsafe_get t i) :: !result
    done;
    !result
  ;;

  (* Convert to a sequence but does not attempt to protect against modification
     in the vec. *)
  let to_sequence_mutable t =
    Sequence.unfold_step ~init:0 ~f:(fun i ->
      if i >= length t then Done else Yield { value = unsafe_get t i; state = i + 1 })
  ;;

  let to_sequence t = to_sequence_mutable (copy t)

  let of_list xs =
    let t = create ~initial_capacity:(List.length xs) () in
    List.iter xs ~f:(push_back t);
    t
  ;;

  let of_array arr = init (Array.length arr) ~f:(fun i -> Array.get arr i)

  let of_sequence seq =
    (* We don't know the length of seq, so we can't set an initial capacity *)
    let t = create () in
    Sequence.iter seq ~f:(push_back t);
    t
  ;;

  let foldi t ~init ~f =
    let r = ref init in
    for i = 0 to max_index t do
      r := f i !r (unsafe_get t i)
    done;
    !r
  ;;

  let fold t ~init ~f =
    let r = ref init in
    for i = 0 to max_index t do
      r := f !r (unsafe_get t i)
    done;
    !r
  ;;

  let foldi_local_accum t ~init:acc ~f =
    let rec aux t i ~acc ~f =
      if i >= length t
      then acc
      else (
        let acc = f i acc (unsafe_get t i) in
        aux t (i + 1) ~acc ~f)
    in
    aux t 0 ~acc ~f
  ;;

  include Blit.Make1 (struct
    type nonrec 'a t = 'a t

    let create_like ~len _t =
      (* Note that even though we [unsafe_create_uninitialized], every time this function
           is called, the [Vec] is immediately blitted with valid values. *)
      Kernel.unsafe_create_uninitialized ~len
    ;;

    let length = length
    let unsafe_blit = unsafe_blit
  end)

  (** Returns the length of the longest prefix for which [f] is true. *)
  let take_while_len t ~f =
    let rec loop i =
      if i >= length t || not (f (get t i)) then i else (loop [@tailcall]) (i + 1)
    in
    loop 0 [@nontail]
  ;;

  let take_while t ~f =
    let len = take_while_len t ~f in
    sub t ~pos:0 ~len
  ;;

  module Inplace = struct
    let sub t ~pos ~len =
      Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length:(length t);
      if pos <> 0 then blit ~src:t ~src_pos:pos ~dst:t ~dst_pos:0 ~len;
      shrink_to t ~len
    ;;

    let take_while t ~f =
      let to_len = take_while_len t ~f in
      shrink_to t ~len:to_len
    ;;

    let filter t ~f =
      let dest = ref 0 in
      for i = 0 to max_index t do
        let x = unsafe_get t i in
        if f x
        then (
          if !dest < i then unsafe_set t !dest x;
          incr dest)
      done;
      let dest = !dest in
      shrink_to t ~len:dest
    ;;

    let map t ~f =
      for i = 0 to max_index t do
        unsafe_set t i (f (unsafe_get t i))
      done
    ;;

    let mapi t ~f =
      for i = 0 to max_index t do
        unsafe_set t i (f i (unsafe_get t i))
      done
    ;;
  end

  let rec forall2__same_length t1 t2 ~f i length =
    if i >= length
    then true
    else
      f (unsafe_get t1 i) (unsafe_get t2 i)
      && forall2__same_length t1 t2 ~f (i + 1) length
  ;;

  let equal equal t t' =
    if length t <> length t'
    then false
    else forall2__same_length t t' ~f:equal 0 (length t)
  ;;

  let clear t = if length t > 0 then shrink_to t ~len:0

  let clear_imm (t : 'a t) (proof : 'a Type_immediacy.Always.t) =
    if length t > 0 then shrink_to_imm t proof ~len:0
  ;;

  let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) t =
    let t = to_list t in
    [%sexp (t : a list)]
  ;;

  let is_empty t = length t = 0

  let exists t ~f =
    let i = ref 0 in
    let n = length t in
    let result = ref false in
    while !i < n && not !result do
      if f (unsafe_get t !i) then result := true else incr i
    done;
    !result
  ;;

  let for_all t ~f =
    let i = ref 0 in
    let n = length t in
    let result = ref true in
    while !i < n && !result do
      if f (unsafe_get t !i) then incr i else result := false
    done;
    !result
  ;;

  let mem t a ~equal = (exists [@inlined hint]) t ~f:(equal a) [@nontail]
  let count t ~f = Container.count ~fold t ~f
  let sum module_ t ~f = Container.sum ~fold module_ t ~f

  (* The code for [find] and [find_exn] would be simpler (wouldn't involve threading
     through [max_index]) if we iterated backward, but we iterate forward to be consistent
     with other containers. *)

  let rec find' t ~f ~max_index i =
    if i > max_index
    then None
    else (
      let x = unsafe_get t i in
      if f x then Some x else find' t ~f ~max_index (i + 1))
  ;;

  let[@cold] raise__not_found () =
    raise (Base.Not_found_s [%message "Vec.find_exn: not found"])
  ;;

  let rec find_exn' t ~f ~max_index i =
    if i > max_index
    then raise__not_found ()
    else (
      let x = unsafe_get t i in
      if f x then x else find_exn' t ~f ~max_index (i + 1))
  ;;

  let find t ~f = find' t ~f ~max_index:(max_index t) 0
  let find_exn t ~f = find_exn' t ~f ~max_index:(max_index t) 0

  let rec findi' t ~f ~max_index i =
    if i > max_index
    then None
    else (
      let x = unsafe_get t i in
      if f x then Some (i, x) else findi' t ~f ~max_index (i + 1))
  ;;

  let findi t ~f = findi' t ~f ~max_index:(max_index t) 0

  let find_and_remove t ~f =
    match findi t ~f with
    | None -> None
    | Some (i, found) ->
      remove_exn t i;
      Some found
  ;;

  let rec find_map' t ~f ~max_index i =
    if i > max_index
    then None
    else (
      match f (unsafe_get t i) with
      | None -> find_map' t ~f ~max_index (i + 1)
      | some -> some)
  ;;

  let find_map t ~f = find_map' t ~f ~max_index:(max_index t) 0

  let rec fold_result' t ~f ~acc ~max_index i =
    if i > max_index
    then Ok acc
    else (
      match f acc (unsafe_get t i) with
      | Ok acc -> fold_result' t ~f ~max_index (i + 1) ~acc
      | err -> err)
  ;;

  let fold_result t ~init ~f = fold_result' t ~f ~acc:init ~max_index:(max_index t) 0

  let rec fold_until' t ~f ~acc ~finish ~max_index i =
    if i > max_index
    then finish acc
    else (
      match (f acc (unsafe_get t i) : _ Continue_or_stop.t) with
      | Stop s -> s
      | Continue acc -> fold_until' t ~f ~max_index (i + 1) ~acc ~finish)
  ;;

  let fold_until t ~init ~f ~finish =
    fold_until' t ~f ~acc:init ~finish ~max_index:(max_index t) 0
  ;;

  let max_elt t ~compare =
    if is_empty t
    then None
    else (
      let max = ref (unsafe_get t 0) in
      for i = 1 to max_index t do
        let x = unsafe_get t i in
        let max' = !max in
        max := if compare max' x < 0 then x else max'
      done;
      Some !max)
  ;;

  let min_elt t ~compare =
    if is_empty t
    then None
    else (
      let min = ref (unsafe_get t 0) in
      for i = 1 to max_index t do
        let x = unsafe_get t i in
        let min' = !min in
        min := if compare min' x > 0 then x else min'
      done;
      Some !min)
  ;;

  let to_array t = Array.init (length t) ~f:(unsafe_get t)
  let t_of_sexp a_of_sexp t = of_list ([%of_sexp: a list] t)

  let compare cmp t1 t2 =
    let len1 = length t1 in
    let len2 = length t2 in
    let min_len = Int.min len1 len2 in
    let result = ref 0 in
    let i = ref 0 in
    while !i < min_len && !result = 0 do
      result := cmp (unsafe_get t1 !i) (unsafe_get t2 !i);
      i := !i + 1
    done;
    if !result = 0 then Int.compare len1 len2 else !result
  ;;

  let unsafe_swap t i j =
    let e = unsafe_get t i in
    unsafe_set t i (unsafe_get t j);
    unsafe_set t j e
  ;;

  let swap t i j =
    check_index t i ~op:"swap";
    check_index t j ~op:"swap";
    unsafe_swap t i j
  ;;

  let swap_to_last_and_pop t i =
    check_index t i ~op:"swap_to_last_and_pop";
    unsafe_swap t i (max_index t);
    pop_back_exn t
  ;;

  module Stable = struct
    module V1 = struct
      type nonrec 'a t = 'a t [@@deriving compare, sexp]

      include Bin_prot.Utils.Make_iterable_binable1 (struct
        type nonrec 'a t = 'a t
        type 'a el = 'a [@@deriving bin_io]

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "2ec1d047-7cf8-49bc-991b-0badd17d8359"
        ;;

        let module_name = Some "Vec"
        let init ~len ~next = init len ~f:(fun _ -> next ())
        let iter = iter
        let length = length
      end)
    end
  end
end

include With_integer_index

module type S = Vec_intf.S

module Make (M : Intable.S) = struct
  include With_integer_index

  let[@inline always] unsafe_get t index = unsafe_get t (M.to_int_exn index)
  let get t index = get t (M.to_int_exn index)
  let maybe_get t index = maybe_get t (M.to_int_exn index)
  let maybe_get_local t index = maybe_get_local t (M.to_int_exn index)
  let[@inline always] unsafe_set t index = unsafe_set t (M.to_int_exn index)
  let set t index = set t (M.to_int_exn index)
  let next_free_index t = next_free_index t |> M.of_int_exn

  let foldi t ~init ~f =
    (foldi [@inlined hint]) t ~init ~f:(fun [@inline] int accum x ->
      f (M.of_int_exn int) accum x) [@nontail]
  ;;

  let foldi_local_accum t ~init ~f =
    (foldi_local_accum [@inlined hint]) t ~init ~f:(fun [@inline] int accum x ->
      f (M.of_int_exn int) accum x) [@nontail]
  ;;

  let iteri t ~f =
    (iteri [@inlined hint]) t ~f:(fun [@inline] int x -> f (M.of_int_exn int) x) [@nontail]
  ;;

  let push_back_index t element = push_back_index t element |> M.of_int_exn

  let to_alist t =
    (* We could do:
       {[
         to_alist t |> List.map ~f:(fun (i, x) -> M.of_int_exn i, x)
       ]}

       at the expense of an extra allocation. This is a bit more copy-pasty,
       but avoids that.
    *)
    let result = ref [] in
    for i = max_index t downto 0 do
      let m = M.of_int_exn i in
      result := (m, unsafe_get t m) :: !result
    done;
    !result
  ;;

  let grow_to_include t idx ~default = grow_to_include t (M.to_int_exn idx) ~default

  let grow_to' t ~len ~default =
    grow_to' t ~len ~default:(fun [@inline] idx -> default (M.of_int_exn idx))
  ;;

  let grow_to_include' t idx ~default =
    grow_to_include' t (M.to_int_exn idx) ~default:(fun [@inline] idx ->
      default (M.of_int_exn idx))
  ;;

  module Inplace = struct
    include Inplace

    let sub t ~pos ~len = sub t ~pos:(M.to_int_exn pos) ~len
    let mapi t ~f = mapi t ~f:(fun [@inline] int x -> f (M.of_int_exn int) x) [@nontail]
  end

  let swap t index1 index2 = swap t (M.to_int_exn index1) (M.to_int_exn index2)
  let swap_to_last_and_pop t index = swap_to_last_and_pop t (M.to_int_exn index)
end
