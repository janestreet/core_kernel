open Core.Std
open Core_bench.Std

module Re2 = Re2.Std.Re2

module type Impl = sig
  val module_name : string
  include Hashtbl_intf.Hashtbl
end

let string_of_mode = function
  | `Sequential_keys -> "no collisions"
  | `With_collisions -> "w/ collisions"

let key_fun_of_mode = function
  | `Sequential_keys -> (fun i -> i)
  | `With_collisions -> (fun i -> i * i)

let bench module_name name ~mode ~make ~size f =
  let mode_str = string_of_mode  mode in
  let key_of   = key_fun_of_mode mode in
  let table =
    List.init size ~f:(fun i -> (key_of i, i))
    |> make ~hashable:Int.hashable
  in
  let state = Random.State.make [||] in
  Bench.Test.create
    ~name:(sprintf "%s.%s [%s]" module_name name mode_str)
    (fun () -> f ~state ~key_of table)

let mem_test ~mode (module Impl : Impl) =
  let make ~hashable alist = Impl.of_alist_exn ~hashable alist in
  bench Impl.module_name "mem" ~mode ~make ~size:100_000 (fun ~state ~key_of table ->
    ignore (Impl.mem table (key_of (Random.State.int state 1_000_000)) : bool))

let find_exn_test ~mode (module Impl : Impl) =
  let make ~hashable alist = Impl.of_alist_exn ~hashable alist in
  bench Impl.module_name "find_exn" ~mode ~make ~size:100_000 (fun ~state ~key_of table ->
    ignore (Impl.find_exn table (key_of (Random.State.int state 100_000)) : int))

let set_test ~mode (module Impl : Impl) =
  let make ~hashable alist = Impl.of_alist_exn ~hashable alist in
  bench Impl.module_name "set" ~mode ~make ~size:100_000 (fun ~state ~key_of table ->
    let i = Random.State.int state 100_000 in
    Impl.set table ~key:(key_of i) ~data:i)

let change_test ~mode (module Impl : Impl) =
  let make ~hashable alist = Impl.of_alist_exn ~hashable alist in
  bench Impl.module_name "change" ~mode ~make ~size:100_000 (fun ~state ~key_of table ->
    let i = Random.State.int state 100_000 in
    Impl.change table (key_of i) ~f:(function
      | None   -> Some 100_000
      | Some _ -> None))

let iter_test ~mode (module Impl : Impl) =
  let make ~hashable alist = Impl.of_alist_exn ~hashable alist in
  let size = 1024 in
  let name = sprintf "iteri, size=%s" (Int.to_string_hum size) in
  bench Impl.module_name name ~mode ~make ~size (fun ~state:_ ~key_of:_ table ->
    Impl.iteri table ~f:(fun ~key:_ ~data:_ -> ()))

let fold_test ~mode (module Impl : Impl) =
  let make ~hashable alist = Impl.of_alist_exn ~hashable alist in
  let size = 1024 in
  let name = sprintf "fold, size=%s" (Int.to_string_hum size) in
  bench Impl.module_name name ~mode ~make ~size (fun ~state:_ ~key_of:_ table ->
    Impl.fold table ~init:() ~f:(fun ~key:_ ~data:_ () -> ()))

let merge_test ~mode (module Impl : Impl) =
  let make ~hashable alist =
    let alist1, alist2 = List.split_n alist (List.length alist / 2) in
    Impl.of_alist_exn ~hashable alist1,
    Impl.of_alist_exn ~hashable alist2
  in
  bench Impl.module_name "merge" ~mode ~make ~size:1024 (fun ~state:_ ~key_of:_ (t1,t2) ->
    Impl.merge t1 t2 ~f:(fun ~key:_ x -> Some x)
    |> (ignore : (int,[`Both of int*int|`Left of int|`Right of int]) Impl.t -> unit))

let add_exn_test ~grow ~mode (module Impl : Impl) =
  let grow_str = if grow then "w/ resize" else "no resize" in
  let mode_str = string_of_mode  mode in
  let key_of   = key_fun_of_mode mode in
  let size = 1024 in
  Bench.Test.create
    ~name:(sprintf "%s.add_exn x %s [%s, %s]"
             Impl.module_name
             (Int.to_string_hum size)
             grow_str
             mode_str)
    (fun () ->
       let table = Impl.create ~size ~growth_allowed:grow ~hashable:Int.hashable () in
       for i = 0 to size do
         Impl.add_exn table ~key:(key_of i) ~data:i
       done)

let impls : (module Impl) list =
  [ (module struct let module_name = "       Hashtbl" include Core.Std.Hashtbl end)
  ; (module struct let module_name = "Pooled_hashtbl" include Core.Std.Pooled_hashtbl end)
  ]

let funcs =
  [ mem_test
  ; find_exn_test
  ; set_test
  ; change_test
  ; iter_test
  ; fold_test
  ; merge_test
  ; add_exn_test ~grow:false
  ; add_exn_test ~grow:true
  ]

let modes = [ `Sequential_keys ; `With_collisions ]

let tests =
  let open List.Monad_infix in
  funcs >>= fun func -> impls >>= fun impl -> modes >>| fun mode -> func ~mode impl

let summary = "Benchmarks for hash tables."

let spec =
  let open Command.Spec in
  let regex = Arg_type.create Re2.create_exn in
  step (fun f regexes -> f ~regexes)
  +> flag "-matching" (listed regex) ~doc:"REGEX Select tests matching given regex(es)."

let main (analysis_configs, display_config, mode) ~regexes () =
  match mode with
  | `From_file _ ->
    failwith
      "This executable is for running benchmarks, not analyzing saved measurements."
  | `Run (save_to_file, run_config) ->
    let tests =
      if List.is_empty regexes
      then tests
      else
        List.filter tests ~f:(fun test ->
          List.exists regexes ~f:(fun regex ->
            Re2.matches regex (Bench.Test.name test)))
    in
    Bench.bench ~run_config ~analysis_configs ~display_config ?save_to_file tests

let () = Command.run (Bench.make_command_ext ~summary ~extra_spec:spec ~f:main)
