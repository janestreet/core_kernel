open Std

let%bench_module "sequence" =
  (module struct

    module Init_and_fold_intf = struct
      module type S = sig
        type +'a t
        val init : int -> f:(int -> 'a) -> 'a t
        val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
      end
    end

    module Map_intf = struct
      module type S = sig
        type +'a t
        include Init_and_fold_intf.S with type 'a t := 'a t
        val map : 'a t -> f:('a -> 'b) -> 'b t
      end
    end

    module Map_filter_intf = struct
      module type S = sig
        type +'a t
        include Map_intf.S with type 'a t := 'a t
        val filter : 'a t -> f:('a -> bool) -> 'a t
      end
    end

    module Append_intf = struct
      module type S = sig
        type +'a t
        include Init_and_fold_intf.S with type 'a t := 'a t
        val append : 'a t -> 'a t -> 'a t
        val return : 'a -> 'a t
      end
    end

    module Bind_intf = struct
      module type S = sig
        type +'a t
        include Init_and_fold_intf.S with type 'a t := 'a t
        val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
        val return : 'a -> 'a t
      end
    end

    module Merge_intf = struct
      module type S = sig
        type +'a t
        include Init_and_fold_intf.S with type 'a t := 'a t
        val filter : 'a t -> f:('a -> bool) -> 'a t
        val merge : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> 'a t
      end
    end

    module Iter_intf = struct
      module type S = sig
        type +'a t
        include Init_and_fold_intf.S with type 'a t := 'a t
        val iter : 'a t -> f:('a -> unit) -> unit
      end
    end

    module Take_intf = struct
      module type S = sig
        type +'a t
        include Init_and_fold_intf.S with type 'a t := 'a t
        val take : 'a t -> int -> 'a t
      end
    end

    module Alt_intf = struct
      module type S = sig
        type +'a t
        include Map_filter_intf.S with type 'a t := 'a t
        val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
      end
    end

    let%bench_module "map" =
      (module struct
        let n = 100_000
        ;;

        let bench (module M : Map_intf.S) =
          M.init n ~f:Fn.id
          |> M.map ~f:(fun x -> x * 2)
          |> M.fold ~f:(+) ~init:0
        ;;

        let%bench "list" = bench (module List)
        ;;

        let%bench "list sequence" =
          bench (module struct
            include Sequence
            let init n ~f = List.init n ~f |> Sequence.of_list
          end)
        ;;

        let%bench "sequence" = bench (module Sequence)
        ;;

        let%bench_fun "baseline" =
          let rec loop acc n =
            if n = 0
            then acc
            else loop (acc + 2 * n) (n - 1)
          in
          fun () -> loop 0 n
        ;;
      end)

    let%bench_module "map filter" =
      (module struct
        let n = 100_000
        let n_half = 50_000
        ;;

        let bench (module M : Map_filter_intf.S) =
          M.init n ~f:Fn.id
          |> M.map ~f:(fun x -> x + 1)
          |> M.filter ~f:(fun x -> x < n_half)
          |> M.fold ~init:0 ~f:(+)
        ;;

        let%bench "list" = bench (module List)
        ;;

        let%bench "list sequence" =
          bench (module struct
            include Sequence
            let init n ~f = List.init n ~f |> Sequence.of_list
          end)
        ;;

        let%bench "sequence" = bench (module Sequence)

        let%bench_fun "baseline" =
          let rec loop acc state =
            if state >= n
            then acc
            else
              let newstate = state + 1 in
              let n = state + 1 in
              if n < n_half
              then loop (acc + n) newstate
              else loop acc newstate
          in
          fun () -> loop 0 0
        ;;
      end)

    let%bench_module "append" =
      (module struct
        let n = 1_000
        ;;

        let bench (module M : Append_intf.S) =
          let m = M.init n ~f:Fn.id in
          M.append m m
          |> M.append m
          |> M.append m
          |> M.append m
          |> M.append m
          |> M.fold ~f:(+) ~init:0
        ;;

        let%bench "sequence" = bench (module Sequence)
        ;;

        let%bench "list" = bench (module List)
        ;;

        let bench (module M : Append_intf.S) n =
          let rec go n acc =
            if n = 0
            then acc
            else go (n - 1) (M.append acc (M.return 0))
          in
          fun () ->
            M.return 0
            |> go n
            |> M.fold ~f:(+) ~init:0
        ;;

        let%bench_fun "sequence loop" [@indexed n = [10; 20; 30; 40; 50]] =
          bench (module Sequence) n
        ;;

        let%bench_fun "list loop" [@indexed n = [10; 20; 30; 40; 50]] =
          bench (module List) n
        ;;
      end)

    let%bench_module "bind" =
      (module struct

        let%bench_module "static" =
          (module struct
            let bench (module M : Bind_intf.S) =
              M.init 100 ~f:Fn.id
              |> M.concat_map ~f:(fun x -> M.init 100 ~f:(fun y -> x + y))
              |> M.concat_map ~f:M.return
              |> M.fold ~init:0 ~f:(+)
            ;;

            let%bench "sequence" = bench (module Sequence)
            ;;

            let%bench "list" = bench (module List)
            ;;
          end)

        let%bench_module "dynamic" =
          (module struct
            let bench (module M : Bind_intf.S) n =
              let rec go n acc =
                if n = 0
                then acc
                else go (n - 1) (M.concat_map acc ~f:(M.return))
              in
              fun () ->
                M.init 5 ~f:Fn.id
                |> go n
                |> M.fold ~f:(+) ~init:0
            ;;

            let%bench_fun "sequence" [@indexed n = [10; 20; 30; 40; 50]] =
              bench (module Sequence) n

            let%bench_fun "list" [@indexed n = [10; 20; 30; 40; 50]] =
              bench (module List) n

          end)
    end)

  let%bench_module "generator" =
    (module struct

      let%bench_fun "right assoc" =
        let rec loop i =
          let open Sequence.Generator.Let_syntax in
          if i >= 100_000
          then return ()
          else
            let%bind () = Sequence.Generator.yield (i * 2) in
            loop (i + 1)
        in
        fun () ->
          loop 0
          |> Sequence.Generator.run
          |> Sequence.fold ~f:(+) ~init:0
      ;;

      let%bench_fun "left_assoc" =
        let rec loop i acc =
          let open Sequence.Generator.Monad_infix in
          if i >= 100_000
          then acc
          else loop (i + 1) (acc >>= fun _ -> Sequence.Generator.yield (i * 2))
        in
        fun () ->
          Sequence.Generator.return ()
          |> loop 0
          |> Sequence.Generator.run
          |> Sequence.fold ~f:(+) ~init:0
      ;;

    end)

  let%bench_module "unfold" =
    (module struct
      let%bench_fun "of_list" =
        let l = List.init 100_000 ~f:Fn.id in
        fun () ->
          Sequence.of_list l
          |> Sequence.fold ~f:(+) ~init:0
      ;;

      let%bench_fun "of_list using unfold_step" =
        let l = List.init 100_000 ~f:Fn.id in
        fun () ->
          Sequence.unfold_step ~init:l ~f:(function
            | [] -> Sequence.Step.Done
            | x :: xs -> Sequence.Step.Yield (x, xs))
          |> Sequence.fold ~f:(+) ~init:0
      ;;

      let%bench_fun "of_list using unfold" =
        let l = List.init 100_000 ~f:Fn.id in
        fun () ->
          Sequence.unfold ~init:l ~f:(function
            | [] -> None
            | x :: xs -> Some (x, xs))
          |> Sequence.fold ~f:(+) ~init:0
      ;;
    end)

  let%bench_module "merge" =
    (module struct

      let bench (module M : Merge_intf.S) =
        let s = M.init 100_000 ~f:Fn.id in
        let s1 = M.filter ~f:(fun i -> i % 2 = 0) s in
        let s2 = M.filter ~f:(fun i -> i % 3 = 0) s in
        M.merge s1 s2 ~cmp:(compare)
        |> M.fold ~f:(+) ~init:0
      ;;

      let%bench "sequence" = bench (module Sequence)

      let%bench "list" = bench (module List)

    end)

  let%bench_module "to_list" =
    (module struct
      let%bench_fun "forward" [@indexed n = [200; 400; 600; 800; 1000]] =
        let s = Sequence.init n ~f:Fn.id in
        fun () -> Sequence.to_list s
      ;;

      let%bench_fun "backward" [@indexed n = [200; 400; 600; 800; 1000]]=
        let s = Sequence.init n ~f:Fn.id in
        fun () -> Sequence.to_list_rev s |> List.rev
      ;;
    end)

  let%bench_module "iter" =
    (module struct
      let bench (module M : Iter_intf.S) =
        let r = ref 0 in
        M.init 100_000 ~f:Fn.id
        |> M.iter ~f:(fun x -> r := !r + x)
      ;;

      let%bench "sequence" = bench (module Sequence)
      ;;

      let%bench "list" = bench (module List)
      ;;
    end)

  let%bench_module "take" =
    (module struct
      let bench (module M : Take_intf.S) =
        M.take (M.init 100_000 ~f:Fn.id) 50_000
        |> M.fold ~init:0 ~f:(+)
      ;;

      let%bench "sequence" = bench (module Sequence)
      ;;

      let%bench "list" = bench (module List)
      ;;

    end)

  (* This is here to compare Core's Sequence against another implementation of a similar
     abstraction in the wild. *)
  let%bench_module "alt" =
    (module struct
      module Alt : Alt_intf.S = struct
        type 'a t = f:('a -> unit) -> unit

        let init n ~f ~f:k = for i = 0 to n-1 do k (f i) done
        ;;

        let filter t ~f ~f:k = t ~f:(fun x -> if f x then k x)
        ;;

        let map t ~f ~f:k = t ~f:(fun x -> k (f x))
        ;;

        let fold t ~init ~f =
          let r = ref init in
          t ~f:(fun x -> r := f !r x);
          !r
        ;;

        let concat_map t ~f ~f:k = t ~f:(fun x -> f x ~f:k)
        ;;
      end

      let%bench_module "without concat_map" =
        (module struct
          let n = 100_000
          let n_half = 50_000
          ;;

          let bench (module M : Alt_intf.S) =
            M.init n ~f:Fn.id
            |> M.map ~f:(fun x -> x + 1)
            |> M.filter ~f:(fun n -> n < n_half)
            |> M.fold ~init:0 ~f:(+)
          ;;

          let%bench "sequence" = bench (module Sequence)
          ;;

          let%bench "list" = bench (module List)
          ;;

          let%bench "alt" = bench (module Alt)
          ;;

          (* [baseline] reflects how [Alt] optimizes. *)
          let%bench "baseline" =
            let r = ref 0 in
            for i = 0 to n-1 do
              let n = i in
              let n = n + 1 in
              if n < n_half
              then r := !r + n
            done; !r

        end)

      let%bench_module "with concat_map" =
        (module struct
          (* [List.concat_map] can't handle [n = 100_000] in a reasonable amount of time. *)
          let n = 1_000
          ;;
          let n_half = 500
          ;;

          let bench (module M : Alt_intf.S) =
            M.init n ~f:Fn.id
            |> M.map ~f:(fun x -> x + 1)
            |> M.filter ~f:(fun n -> n < n_half)
            |> M.concat_map ~f:(fun n -> M.init n ~f:Fn.id)
            |> M.fold ~init:0 ~f:(+)
          ;;

          let%bench "sequence" = bench (module Sequence)
          ;;

          let%bench "list" = bench (module List)
          ;;

          let%bench "alt" = bench (module Alt)
          ;;

          let%bench_fun "baseline" =
            let rec loop acc state =
              if state >= n
              then acc
              else
                let newstate = state + 1 in
                let n = state + 1 in
                if n < n_half
                then
                  let rec loop_in acc state_in =
                    if state_in >= n
                    then acc
                    else
                      loop_in (acc + state_in) (state_in + 1)
                  in
                  loop (loop_in acc 0) newstate
                else loop acc newstate
            in
            fun () -> loop 0 0
          ;;

        end)
    end)

  let%bench "interleave" =
    Sequence.init 100 ~f:(fun n -> Sequence.init n ~f:Fn.id)
    |> Sequence.interleave
    |> Sequence.fold ~init:0 ~f:(+)
  ;;

  let%bench "memoize" =
    Sequence.init 100_000 ~f:(fun i -> i + i)
    |> Sequence.memoize
    |> Sequence.fold ~init:0 ~f:(+)
  ;;

  let%bench "cartesian" =
    Sequence.init 100 ~f:Fn.id
    |> Sequence.cartesian_product (Sequence.init 100 ~f:Fn.id)
    |> Sequence.fold ~init:0 ~f:(fun acc (a, b) -> acc + a + b)
  ;;

  let%bench "delayed_fold" =
    Sequence.init 100_000 ~f:Fn.id
    |> Sequence.delayed_fold
         ~init:0
         ~f:(fun acc a ~k -> if a < 90_000 then k acc
              else acc + a)
         ~finish:(fun acc -> acc)
  ;;

  let%bench "split" =
    let (_, t) = Sequence.split_n (Sequence.init 100_000 ~f:Fn.id) 90_000 in
    Sequence.fold t ~init:0 ~f:(+)
  ;;

end)
