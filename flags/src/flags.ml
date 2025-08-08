open! Core
include Flags_intf

(* To allow [create] to be eagerly inlined, move this exception with macro expansion to
   its own function and mark it [@cold] so that it isn't inlined. *)
let[@cold] raise_invalid_bit n =
  failwiths "Flags.create got invalid ~bit (must be between 0 and 62)" n [%sexp_of: int]
;;

let create ~bit:n =
  if n < 0 || n > 62 then raise_invalid_bit n;
  Int63.shift_left Int63.one n
;;

module%template Make (M : sig
    include Make_arg
  end) =
struct
  type t = Int63.t [@@deriving bin_io, hash, typerep]

  let of_int t = Int63.of_int t
  let to_int_exn t = Int63.to_int_exn t
  let empty = Int63.zero
  let is_empty t = Int63.equal t empty
  let ( + ) a b = Int63.bit_or a b
  let ( - ) a b = Int63.bit_and a (Int63.bit_not b)
  let[@inline] intersect a b = Int63.bit_and a b
  let all = List.fold M.known ~init:empty ~f:(fun acc (flag, _) -> acc + flag)
  let complement a = all - a
  let is_subset t ~of_ = Int63.( = ) t (intersect t of_)
  let do_intersect t1 t2 = Int63.( <> ) (Int63.bit_and t1 t2) Int63.zero
  let are_disjoint t1 t2 = Int63.( = ) (Int63.bit_and t1 t2) Int63.zero

  include
    Quickcheckable.Of_quickcheckable [@mode p]
      (Int63)
      (struct
        type nonrec t = t

        let of_quickcheckable t = intersect t all
        let to_quickcheckable t = t
      end)

  let error message a sexp_of_a =
    let e = Error.create message a sexp_of_a in
    if M.should_print_error then eprintf "%s\n%!" (Sexp.to_string_hum (Error.sexp_of_t e));
    Error.raise e
  ;;

  let known =
    if M.remove_zero_flags
    then List.filter ~f:(fun (n, _) -> not (Int63.equal n Int63.zero)) M.known
    else M.known
  ;;

  let any_intersecting flags =
    let rec loop l acc =
      match l with
      | [] -> false
      | (flag, _) :: l -> if do_intersect flag acc then true else loop l (acc + flag)
    in
    loop flags empty
  ;;

  let () =
    if not M.allow_intersecting
    then
      if any_intersecting known
      then (
        let rec check l ac =
          match l with
          | [] -> ac
          | (flag, name) :: l ->
            let bad = List.filter l ~f:(fun (flag', _) -> do_intersect flag flag') in
            let ac = if List.is_empty bad then ac else (flag, name, bad) :: ac in
            check l ac
        in
        let bad = check known [] in
        assert (not (List.is_empty bad));
        error
          "Flags.Make got intersecting flags"
          bad
          [%sexp_of: (Int63.t * string * (Int63.t * string) list) list])
  ;;

  let () =
    let bad = List.filter known ~f:(fun (flag, _) -> Int63.equal flag Int63.zero) in
    if not (List.is_empty bad)
    then
      error "Flag.Make got flags with no bits set" bad [%sexp_of: (Int63.t * string) list]
  ;;

  type sexp_format = string list [@@deriving sexp]

  type sexp_format_with_unrecognized_bits = string list * [ `unrecognized_bits of string ]
  [@@deriving sexp]

  let to_flag_list =
    (* We reverse [known] so that the fold below accumulates from right to left, giving a
       final list with elements in the same order as [known]. *)
    let known = List.rev known in
    fun t ->
      List.fold known ~init:(t, []) ~f:(fun (t, flag_names) (flag, flag_name) ->
        if Int63.equal (Int63.bit_and t flag) flag
        then t - flag, flag_name :: flag_names
        else t, flag_names)
  ;;

  let sexp_of_t t =
    let to_unsigned_hex_string x =
      Int64.(max_value land Int63.to_int64 x) |> Int64.Hex.to_string
    in
    let leftover, flag_names = to_flag_list t in
    if Int63.equal leftover empty
    then [%sexp_of: sexp_format] flag_names
    else
      [%sexp_of: sexp_format_with_unrecognized_bits]
        (flag_names, `unrecognized_bits (to_unsigned_hex_string leftover))
  ;;

  let known_by_name =
    String.Map.of_iteri_exn ~iteri:(fun ~f ->
      List.iter known ~f:(fun (mask, name) -> f ~key:name ~data:mask) [@nontail])
  ;;

  let t_of_sexp (sexp : Sexp.t) =
    let of_unsigned_hex_string s = Int64.Hex.of_string s |> Int63.of_int64_trunc in
    let restore_int_of_flags_sexp flags =
      List.fold
        (flags |> [%of_sexp: sexp_format])
        ~init:empty
        ~f:(fun t name ->
          match Map.find known_by_name name with
          | Some mask -> t + mask
          | None ->
            of_sexp_error (sprintf "Flags.t_of_sexp got unknown name: %s" name) sexp)
    in
    match sexp with
    | Sexp.List [ Sexp.List flags; Sexp.List unrecognized ] ->
      (match unrecognized with
       | [ Sexp.Atom "unrecognized_bits"; Sexp.Atom num ] ->
         restore_int_of_flags_sexp (Sexp.List flags) + of_unsigned_hex_string num
       | _ ->
         raise_s
           [%message
             "Of_sexp_error: sexp format does not match any recognized format"
               (sexp : Sexp.t)])
    | Sexp.List flags -> restore_int_of_flags_sexp (Sexp.List flags)
    | Sexp.Atom _ -> raise_s [%message "Of_sexp_error: list needed" (sexp : Sexp.t)]
  ;;

  (* total order such that [subset a b] implies [a <= b] *)
  let%template compare t u =
    (* This is the same as {| Int63.(i bit_xor (one shift_left 62)) |} *)
    let flip_top_bit i = Int63.( + ) i Int63.min_value in
    Int63.compare (flip_top_bit t) (flip_top_bit u)
  [@@mode local]
  ;;

  (* we only use [compare [@mode local]] below *)
  let compare = `unused
  let `unused = compare

  include%template Comparable.Make [@mode local p] (struct
      type nonrec t = t [@@deriving sexp, compare ~localize, hash]
    end)

  (* [Comparable.Make] turns [equal] into a function call to [compare] rather than the
     much simpler (and equally correct) [Int63.equal]. Restore it, as well as (=) and
     (<>). *)
  let%template equal = (Int63.equal [@mode m]) [@@mode m = (local, global)]
  let ( = ) = Int63.( = )
  let ( <> ) = Int63.( <> )

  module Unstable = struct
    type nonrec t = t [@@deriving bin_io, compare ~localize, equal ~localize, sexp]
  end
end
[@@modality p = (nonportable, portable)]

module%template Make_binable (M : sig
    include Make_arg
  end) =
  Make [@mode p] (M)
[@@modality p = (nonportable, portable)]
