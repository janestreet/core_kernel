open Std

open Core_map_intf

let%bench_module "Map.to_sequence" = (module struct
  let gen_test ~size ~from ~to_ =
    let map =
      List.init size ~f:(fun i -> string_of_int i, i) |> String.Map.of_alist_exn
    in
    fun () ->
      let seq =
        Map.to_sequence map
          ~keys_greater_or_equal_to:from
          ~keys_less_or_equal_to:to_
      in
      Sequence.iter seq ~f:ignore

  let%bench_fun "small-less" = gen_test ~size:      100 ~from:"45"     ~to_:"55"
  let%bench_fun "small-more" = gen_test ~size:      100 ~from: "5"     ~to_:"95"
  let%bench_fun "big-less"   = gen_test ~size:1_000_000 ~from:"200000" ~to_:"200050"
  let%bench_fun "big-more"   = gen_test ~size:1_000_000 ~from:"20"     ~to_:"900000"
end)


let%bench_module "Map.bin_read_t" =
  (module struct

    module Make_binable_using_comparator_old (Key' : sig
        type t [@@deriving bin_io, sexp]
        include Comparator.S with type t := t
      end) = struct

      include Core_map.Make_using_comparator (Key')

      include Bin_prot.Utils.Make_iterable_binable1 (struct
          type nonrec 'v t = 'v t
          type 'v el = Key'.t * 'v [@@deriving bin_io]
          let _ = bin_el
          let module_name = Some "Core.Std.Map"
          let length = length
          let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))
          let init ~len ~next =
            let rec loop next acc = function
              | 0 -> acc
              | n ->
                let (key,data) = next () in
                if mem acc key
                then failwith "Map.bin_read_t_: duplicate element in map"
                else loop next (add ~key ~data acc) (pred n)
            in loop next empty len
          ;;
        end)

    end

    module Make_binable_using_comparator_with_intermediate_array (Key' : sig
        type t [@@deriving bin_io, sexp, compare]
        include Comparator.S with type t := t
      end) = struct

      include Core_map.Make_using_comparator (Key')

      include Bin_prot.Utils.Make_iterable_binable1 (struct
          type nonrec 'v t = 'v t
          type 'v el = Key'.t * 'v [@@deriving bin_io]
          let _ = bin_el
          let module_name = Some "Core.Std.Map"
          let length = length
          let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))
          type 'v acc = { mutable elements : 'v el array; count : int }
          let init ~len ~next =
            let acc = { elements = [||]; count = len } in
            for i = 0 to len - 1 do
              let x = next () in
              if i = 0 then
                acc.elements <- Array.create ~len:acc.count x
              else
                acc.elements.(i) <- x;
            done;
            let rec array_is_sorted_no_dup prev arr idx =
              if idx = Array.length arr then
                true
              else
                let key, _ = arr.(idx) in
                Key'.compare prev key < 0 &&
                array_is_sorted_no_dup key arr (idx + 1)
            in
            if acc.count = 0 then
              empty
            else if array_is_sorted_no_dup (fst acc.elements.(0)) acc.elements 1 then
              of_sorted_array_unchecked acc.elements
            else begin
              Array.sort ~cmp:(fun (k1, _) (k2, _) -> Key'.compare k1 k2)
                acc.elements;
              of_sorted_array_unchecked acc.elements
            end
        end)
    end

    module Current  = Core_map.Make_binable_using_comparator(String)
    module Previous = Make_binable_using_comparator_old(String)
    module Through_array = Make_binable_using_comparator_with_intermediate_array(String)

    let gen_test read_t ~size =
      let map =
        List.init size ~f:(fun i -> string_of_int i, i) |> String.Map.of_alist_exn
      in
      let buf = Bin_prot.Common.create_buf (1_000_0000 * 8 + 8) in
      let pos = 0 in
      let _pos = String.Map.bin_write_t Int.bin_write_t buf ~pos map in
      fun () ->
        let pos_ref = ref pos in
        ignore (read_t Int.bin_read_t buf ~pos_ref : Int.t Current.t)

    let%bench_fun "tiny (opt)"  = gen_test Current.bin_read_t       ~size:        5
    let%bench_fun "tiny"        = gen_test Previous.bin_read_t      ~size:        5
    let%bench_fun "tiny (arr)"  = gen_test Through_array.bin_read_t ~size:        5
    let%bench_fun "small (opt)" = gen_test Current.bin_read_t       ~size:      300
    let%bench_fun "small"       = gen_test Previous.bin_read_t      ~size:      300
    let%bench_fun "small (arr)" = gen_test Through_array.bin_read_t ~size:      300
    let%bench_fun "big (opt)"   = gen_test Current.bin_read_t       ~size:1_000_000
    let%bench_fun "big"         = gen_test Previous.bin_read_t      ~size:1_000_000
    let%bench_fun "big (arr)"   = gen_test Through_array.bin_read_t ~size:1_000_000

  end)
