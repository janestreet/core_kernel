open Core.Core_stable

module Stable = struct
  module V3 = struct
    module T = struct
      type nonrec 'a t = ( :: ) of 'a * 'a list [@@deriving compare, equal, hash]

      let to_list (hd :: tl) : _ list = hd :: tl

      let of_list_exn : _ list -> _ t = function
        | [] -> Core.raise_s [%message "Nonempty_list.of_list_exn: empty list"]
        | hd :: tl -> hd :: tl
      ;;
    end

    include T

    module Format = struct
      type 'a t = 'a list [@@deriving bin_io, sexp, stable_witness]
    end

    include
      Binable.Of_binable1.V2
        (Format)
        (struct
          include T

          let to_binable = to_list
          let of_binable = of_list_exn

          let caller_identity =
            Bin_prot.Shape.Uuid.of_string "9a63aaee-82e0-11ea-8fb6-aa00005c6184"
          ;;
        end)

    include
      Sexpable.Of_sexpable1.V1
        (Format)
        (struct
          include T

          let to_sexpable = to_list
          let of_sexpable = of_list_exn
        end)

    let t_sexp_grammar (type a) ({ untyped = element } : [%sexp_grammar: a])
      : [%sexp_grammar: a t]
      =
      { untyped = List (Cons (element, Many element)) }
    ;;

    let stable_witness (type a) : a Stable_witness.t -> a t Stable_witness.t =
      fun witness ->
      let module Stable_witness = Stable_witness.Of_serializable1 (Format) (T) in
      Stable_witness.of_serializable Format.stable_witness of_list_exn to_list witness
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: int t];
      [%expect {| eaa5c1535ea5c1691291b3bdbbd7b014 |}]
    ;;
  end

  module V2 = struct
    module T = struct
      type nonrec 'a t = 'a V3.t = ( :: ) of 'a * 'a list
      [@@deriving compare, equal, hash]

      let sexp_of_t = V3.sexp_of_t
      let t_of_sexp = V3.t_of_sexp
    end

    include T

    module Record_format = struct
      type 'a t =
        { hd : 'a
        ; tl : 'a list
        }
      [@@deriving bin_io, compare, stable_witness]

      let of_nonempty_list (hd :: tl) = { hd; tl }
      let to_nonempty_list { hd; tl } = hd :: tl
    end

    include
      Binable.Of_binable1.V1 [@alert "-legacy"]
        (Record_format)
        (struct
          include T

          let to_binable = Record_format.of_nonempty_list
          let of_binable = Record_format.to_nonempty_list
        end)

    let stable_witness (type a) : a Stable_witness.t -> a t Stable_witness.t =
      fun witness ->
      let module Stable_witness = Stable_witness.Of_serializable1 (Record_format) (T) in
      Stable_witness.of_serializable
        Record_format.stable_witness
        Record_format.to_nonempty_list
        Record_format.of_nonempty_list
        witness
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: int t];
      [%expect {| 2aede2e9b03754f5dfa5f1a61877b330 |}]
    ;;
  end

  module V1 = struct
    module T = struct
      type 'a t = 'a V2.t = ( :: ) of 'a * 'a list [@@deriving compare, equal]

      let sexp_of_t = V2.sexp_of_t
      let t_of_sexp = V2.t_of_sexp
    end

    include T

    module Pair_format = struct
      type 'a t = 'a * 'a list [@@deriving bin_io, compare, stable_witness]

      let of_nonempty_list (hd :: tl) = hd, tl
      let to_nonempty_list (hd, tl) = hd :: tl
    end

    include
      Binable.Of_binable1.V1 [@alert "-legacy"]
        (Pair_format)
        (struct
          include T

          let to_binable = Pair_format.of_nonempty_list
          let of_binable = Pair_format.to_nonempty_list
        end)

    let stable_witness (type a) : a Stable_witness.t -> a t Stable_witness.t =
      fun witness ->
      let module Stable_witness = Stable_witness.Of_serializable1 (Pair_format) (T) in
      Stable_witness.of_serializable
        Pair_format.stable_witness
        Pair_format.to_nonempty_list
        Pair_format.of_nonempty_list
        witness
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: int t];
      [%expect {| f27871ef428aef2925f18d6be687bf9c |}]
    ;;
  end
end

open Core
module Unstable = Stable.V3

module T' = struct
  type 'a t = 'a Stable.V3.t = ( :: ) of 'a * 'a list
  [@@deriving compare, equal, hash, quickcheck, typerep, bin_io, globalize]

  let sexp_of_t = Stable.V3.sexp_of_t
  let t_of_sexp = Stable.V3.t_of_sexp
  let t_sexp_grammar = Stable.V3.t_sexp_grammar
  let to_list = Stable.V3.to_list
  let of_list_exn = Stable.V3.of_list_exn
  let hd (hd :: _) = hd
  let tl (_ :: tl) = tl

  let of_list = function
    | [] -> None
    | hd :: tl -> Some (hd :: tl)
  ;;

  let of_list_error = function
    | [] -> Core.error_s [%message "empty list"]
    | hd :: tl -> Ok (hd :: tl)
  ;;

  let fold (hd :: tl) ~init ~f = List.fold tl ~init:(f init hd) ~f
  let foldi = `Define_using_fold

  let iter =
    `Custom
      (fun (hd :: tl) ~f ->
        f hd;
        List.iter tl ~f)
  ;;

  let iteri = `Define_using_fold
  let length = `Custom (fun (_ :: tl) -> 1 + List.length tl)
end

include T'
include Comparator.Derived (T')

include struct
  let is_empty _ = false

  (* [Container.Make] would fold through the tail and re-cons every elt. *)
  let to_list = to_list

  module From_indexed_container_make = Indexed_container.Make (T')
  open From_indexed_container_make

  let mem = mem
  let length = length
  let iter = iter
  let fold = fold
  let fold_result = fold_result
  let fold_until = fold_until
  let exists = exists
  let for_all = for_all
  let count = count
  let sum = sum
  let find = find
  let find_map = find_map
  let to_array = to_array
  let min_elt = min_elt
  let max_elt = max_elt
  let iteri = iteri
  let find_mapi = find_mapi
  let findi = findi
  let counti = counti
  let for_alli = for_alli
  let existsi = existsi
  let foldi = foldi
end

let invariant f t = iter t ~f
let create hd tl = hd :: tl
let singleton hd = [ hd ]
let cons x (hd :: tl) = x :: hd :: tl

let nth (hd :: tl) n =
  match n with
  | 0 -> Some hd
  | n -> List.nth tl (n - 1)
;;

let nth_exn t n =
  match nth t n with
  | None ->
    invalid_argf "Nonempty_list.nth_exn %d called on list of length %d" n (length t) ()
  | Some a -> a
;;

let mapi (hd :: tl) ~f =
  (* Being overly cautious about evaluation order *)
  let hd = f 0 hd in
  hd :: List.mapi tl ~f:(fun i x -> f (i + 1) x)
;;

let filter_map (hd :: tl) ~f : _ list =
  match f hd with
  | None -> List.filter_map tl ~f
  | Some hd -> hd :: List.filter_map tl ~f
;;

let filter_mapi (hd :: tl) ~f : _ list =
  let hd = f 0 hd in
  let[@inline always] f i x = f (i + 1) x in
  match hd with
  | None -> List.filter_mapi tl ~f [@nontail]
  | Some hd -> hd :: List.filter_mapi tl ~f
;;

let filter (hd :: tl) ~f : _ list =
  match f hd with
  | false -> List.filter tl ~f
  | true -> hd :: List.filter tl ~f
;;

let filteri (hd :: tl) ~f : _ list =
  let include_hd = f 0 hd in
  let[@inline always] f i x = f (i + 1) x in
  match include_hd with
  | false -> List.filteri tl ~f [@nontail]
  | true -> hd :: List.filteri tl ~f
;;

let map t ~f = mapi t ~f:(fun (_ : int) x -> f x) [@nontail]

let map2 t1 t2 ~f : _ List.Or_unequal_lengths.t =
  match List.map2 (to_list t1) (to_list t2) ~f with
  | Ok x -> Ok (of_list_exn x)
  | Unequal_lengths -> Unequal_lengths
;;

let map2_exn t1 t2 ~f = List.map2_exn (to_list t1) (to_list t2) ~f |> of_list_exn
let reduce (hd :: tl) ~f = List.fold ~init:hd tl ~f

let reverse (hd :: tl) =
  let rec loop acc x xs =
    match xs with
    | [] -> x :: acc
    | y :: ys -> loop (x :: acc) y ys
  in
  loop [] hd tl
;;

let append (hd :: tl) l = hd :: List.append tl l

include Monad.Make_local (struct
  type nonrec 'a t = 'a t

  let return hd = [ hd ]
  let map = `Custom map

  let bind (hd :: tl) ~f =
    let f_hd = f hd in
    append f_hd (List.concat_map tl ~f:(fun x -> to_list (f x)))
  ;;
end)

let unzip ((hd1, hd2) :: tl) =
  let tl1, tl2 = List.unzip tl in
  hd1 :: tl1, hd2 :: tl2
;;

let concat t = bind t ~f:Fn.id
let concat_map = bind

let zip t1 t2 : _ List.Or_unequal_lengths.t =
  match List.zip (to_list t1) (to_list t2) with
  | Ok x -> Ok (of_list_exn x)
  | Unequal_lengths -> Unequal_lengths
;;

let zip_exn t1 t2 = List.zip_exn (to_list t1) (to_list t2) |> of_list_exn
let last (hd :: tl) = List.fold tl ~init:hd ~f:(fun _ elt -> elt)

let drop_last (hd :: tl) =
  match List.drop_last tl with
  | None -> []
  | Some l -> hd :: l
;;

let to_sequence t =
  (* [to_list] just performs one [::], so this sequence is created with only constant
     up-front work *)
  Sequence.of_list (to_list t)
;;

let sort t ~compare = List.sort (to_list t) ~compare |> of_list_exn
let stable_sort t ~compare = List.stable_sort (to_list t) ~compare |> of_list_exn
let dedup_and_sort t ~compare = List.dedup_and_sort ~compare (to_list t) |> of_list_exn
let permute ?random_state t = List.permute ?random_state (to_list t) |> of_list_exn

let min_elt' (hd :: tl) ~compare =
  List.fold tl ~init:hd ~f:(fun min elt -> if compare min elt > 0 then elt else min) [@nontail
                                                                                       ]
;;

let max_elt' t ~compare = min_elt' t ~compare:(fun x y -> compare y x) [@nontail]

let map_add_multi map ~key ~data =
  Map.update map key ~f:(function
    | None -> singleton data
    | Some t -> cons data t)
;;

let map_of_container_multi fold container ~comparator =
  fold container ~init:(Map.empty comparator) ~f:(fun acc (key, data) ->
    map_add_multi acc ~key ~data)
;;

let map_of_alist_multi alist = map_of_container_multi List.fold alist
let map_of_sequence_multi sequence = map_of_container_multi Sequence.fold sequence
let fold_nonempty (hd :: tl) ~init ~f = List.fold tl ~init:(init hd) ~f

let map_of_list_with_key_multi list ~comparator ~get_key =
  List.fold list ~init:(Map.empty comparator) ~f:(fun acc data ->
    let key = get_key data in
    map_add_multi acc ~key ~data)
;;

let fold_right (hd :: tl) ~init:acc ~f =
  let acc = List.fold_right tl ~init:acc ~f in
  f hd acc
;;

let folding_map (hd :: tl) ~init ~f =
  let acc, hd = f init hd in
  hd :: List.folding_map tl ~init:acc ~f
;;

let fold_map (hd :: tl) ~init:acc ~f =
  let acc, hd = f acc hd in
  let acc, tl = List.fold_map tl ~init:acc ~f in
  acc, hd :: tl
;;

let combine_errors t =
  match Result.combine_errors (to_list t) with
  | Ok oks -> Ok (of_list_exn oks)
  | Error errors -> Error (of_list_exn errors)
;;

let combine_errors_unit t =
  match Result.combine_errors_unit (to_list t) with
  | Ok _ as ok -> ok
  | Error errors -> Error (of_list_exn errors)
;;

let combine_or_errors t =
  match Or_error.combine_errors (to_list t) with
  | Ok oks -> Ok (of_list_exn oks)
  | Error _ as e -> e
;;

let combine_or_errors_unit t = to_list t |> Or_error.combine_errors_unit
let validate ~name check t = Validate.list ~name check (to_list t)
let validate_indexed check t = Validate.list_indexed check (to_list t)

let rec rev_append xs acc =
  match (xs : _ Reversed_list.t) with
  | [] -> acc
  | hd :: tl -> rev_append tl (cons hd acc)
;;

let init n ~f =
  if n < 1 then invalid_argf "Nonempty_list.init %d" n ();
  (* [List.init] calls [f] on the highest index first and works its way down.
     We do the same here. *)
  let tl = List.init (n - 1) ~f:(fun i -> f (i + 1)) in
  let hd = f 0 in
  hd :: tl
;;

let cartesian_product t t' =
  List.cartesian_product (to_list t) (to_list t') |> of_list_exn
;;

module Reversed = struct
  type 'a t = ( :: ) of 'a * 'a Reversed_list.t

  let to_rev_list (hd :: tl) : _ Reversed_list.t = hd :: tl
  let rev_append (hd :: tl : _ t) xs = rev_append tl (hd :: xs)
  let rev t = rev_append t []

  let rec rev_map_aux i xs ~f acc =
    match (xs : _ Reversed_list.t) with
    | [] -> acc
    | hd :: tl -> rev_map_aux (i + 1) tl ~f (cons (f i hd) acc)
  ;;

  let rev_mapi (hd :: tl : _ t) ~f = rev_map_aux 1 tl ~f ([ f 0 hd ] : _ T'.t)
  let rev_map t ~f = rev_mapi t ~f:(fun _ x -> f x) [@nontail]
  let cons x t = x :: to_rev_list t

  module With_sexp_of = struct
    type nonrec 'a t = 'a t

    let sexp_of_t sexp_of_a t =
      Reversed_list.With_sexp_of.sexp_of_t sexp_of_a (to_rev_list t)
    ;;
  end

  module With_rev_sexp_of = struct
    type nonrec 'a t = 'a t

    let sexp_of_t sexp_of_a t =
      Reversed_list.With_rev_sexp_of.sexp_of_t sexp_of_a (to_rev_list t)
    ;;
  end
end

let rev' (hd :: tl) =
  List.fold tl ~init:([ hd ] : _ Reversed.t) ~f:(Fn.flip Reversed.cons)
;;

let flag arg_type =
  Command.Param.map_flag
    (Command.Param.one_or_more_as_pair arg_type)
    ~f:(fun (one, more) -> one :: more)
;;

let comma_separated_argtype ?key ?strip_whitespace ?unique_values arg_type =
  arg_type
  |> Command.Param.Arg_type.comma_separated
       ~allow_empty:false
       ?strip_whitespace
       ?unique_values
  |> Command.Param.Arg_type.map ?key ~f:of_list_exn
;;

type 'a nonempty_list = 'a t

(** This relies on the fact that the representation of [List.( :: )] constructor is
    identical to that of [Nonempty_list.( :: )], and that they are each the first
    non-constant constructor in their respective types. *)
module Option = struct
  type 'a t = 'a list
  [@@deriving compare, equal, sexp, sexp_grammar, hash, quickcheck, typerep]

  let none = []
  let some (_ :: _ as value : 'a nonempty_list) : 'a t = Obj.magic value
  let unchecked_value (t : 'a t) : 'a nonempty_list = Obj.magic t
  let is_none t = phys_equal t none
  let is_some t = not (is_none t)
  let to_option = of_list

  let of_option = function
    | None -> none
    | Some value -> some value
  ;;

  let value_exn = function
    | [] -> raise_s [%sexp "Nonempty_list.Option.value_exn: empty list"]
    | _ :: _ as l -> unchecked_value l
  ;;

  let value t ~default = Bool.select (is_none t) default (unchecked_value t)

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end
end
