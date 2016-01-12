open Sexplib.Std
open Bin_prot.Std
include Caml.Gc

module Int = Core_int
module Sexp = Sexplib.Sexp
let sprintf = Printf.sprintf

module Stat = struct
  type pretty_float = float [@@deriving compare, bin_io, sexp]
  let sexp_of_pretty_float f = Sexp.Atom (sprintf "%.2e" f)

  module T = struct
    type t = Caml.Gc.stat = {
      minor_words : pretty_float;
      promoted_words : pretty_float;
      major_words : pretty_float;
      minor_collections : int;
      major_collections : int;
      heap_words : int;
      heap_chunks : int;
      live_words : int;
      live_blocks : int;
      free_words : int;
      free_blocks : int;
      largest_free : int;
      fragments : int;
      compactions : int;
      top_heap_words : int;
      stack_size : int
    } [@@deriving compare, bin_io, sexp, fields]
  end

  include T
  include Comparable.Make(T)
end

module Control = struct
  (* The GC parameters are given as a control record.
     Note that these parameters can also be initialised
     by setting the OCAMLRUNPARAM environment variable.
     See the documentation of ocamlrun. *)
  module T = struct
    type t = Caml.Gc.control = {
      (* The size (in words) of the minor heap. Changing this parameter will trigger a
         minor collection. Default: 32k. *)
      mutable minor_heap_size : int;
      (* The minimum number of words to add to the major heap when increasing it. Default:
         62k. *)
      mutable major_heap_increment : int;
      (* The major GC speed is computed from this parameter. This is the memory that will
         be "wasted" because the GC does not immediatly collect unreachable blocks. It is
         expressed as a percentage of the memory used for live data. The GC will work more
         (use more CPU time and collect blocks more eagerly) if space_overhead is
         smaller. Default: 80. *)
      mutable space_overhead : int;
      (* This value controls the GC messages on standard error output. It is a sum of some
         of the following flags, to print messages on the corresponding events:

       * 0x001 Start of major GC cycle.
       * 0x002 Minor collection and major GC slice.
       * 0x004 Growing and shrinking of the heap.
       * 0x008 Resizing of stacks and memory manager tables.
       * 0x010 Heap compaction.
       * 0x020 Change of GC parameters.
       * 0x040 Computation of major GC slice size.
       * 0x080 Calling of finalisation functions.
       * 0x100 Bytecode executable search at start-up.
       * 0x200 Computation of compaction triggering condition. Default: 0. *)
      mutable verbose : int;
      (* Heap compaction is triggered when the estimated amount of "wasted" memory is more
         than max_overhead percent of the amount of live data. If max_overhead is set to
         0, heap compaction is triggered at the end of each major GC cycle (this setting
         is intended for testing purposes only). If max_overhead >= 1000000, compaction is
         never triggered. Default: 500. *)
      mutable max_overhead : int;
      (* The maximum size of the stack (in words). This is only relevant to the byte-code
         runtime, as the native code runtime uses the operating system's stack. Default:
         256k. *)
      mutable stack_limit : int;
      (** The policy used for allocating in the heap.  Possible values are 0 and 1.  0 is
          the next-fit policy, which is quite fast but can result in fragmentation.  1 is
          the first-fit policy, which can be slower in some cases but can be better for
          programs with fragmentation problems.  Default: 0. *)
      mutable allocation_policy : int;
    } [@@deriving compare, bin_io, sexp, fields]
  end

  include T
  include Comparable.Make(T)
end

let tune ?logger ?minor_heap_size ?major_heap_increment ?space_overhead
      ?verbose ?max_overhead ?stack_limit ?allocation_policy () =
  let module Field = Fieldslib.Field in
  let old_control_params = get () in
  let f opt to_string field =
    let old_value = Field.get field old_control_params in
    match opt with
    | None -> old_value
    | Some new_value ->
      Option.iter logger ~f:(fun f ->
        Printf.ksprintf f "Gc.Control.%s: %s -> %s"
          (Field.name field) (to_string old_value) (to_string new_value));
      new_value
  in
  let new_control_params =
    Control.Fields.map
      ~minor_heap_size:     (f minor_heap_size      string_of_int)
      ~major_heap_increment:(f major_heap_increment string_of_int)
      ~space_overhead:      (f space_overhead       string_of_int)
      ~verbose:             (f verbose              string_of_int)
      ~max_overhead:        (f max_overhead         string_of_int)
      ~stack_limit:         (f stack_limit          string_of_int)
      ~allocation_policy:   (f allocation_policy    string_of_int)
  in
  set new_control_params
;;

module Allocation_policy = struct
  type t =
    | Next_fit
    | First_fit

  let to_int = function
    | Next_fit  -> 0
    | First_fit -> 1
end

let disable_compaction ?logger ~allocation_policy () =
  let allocation_policy =
    match allocation_policy with
    | `Don't_change  -> None
    | `Set_to policy -> Some (Allocation_policy.to_int policy)
  in
  (* The value 1_000_000, according to
     http://caml.inria.fr/pub/docs/manual-ocaml-4.02/libref/Gc.html
     will disable compactions.
  *)
  tune ?logger ?allocation_policy ~max_overhead:1_000_000 ();
;;

external minor_words : unit -> int = "core_kernel_gc_minor_words"
external major_words : unit -> int = "core_kernel_gc_major_words" "noalloc"
external promoted_words : unit -> int = "core_kernel_gc_promoted_words" "noalloc"
external minor_collections : unit -> int = "core_kernel_gc_minor_collections" "noalloc"
external major_collections : unit -> int = "core_kernel_gc_major_collections" "noalloc"
external heap_words : unit -> int = "core_kernel_gc_heap_words" "noalloc"
external heap_chunks : unit -> int = "core_kernel_gc_heap_chunks" "noalloc"
external compactions : unit -> int = "core_kernel_gc_compactions" "noalloc"
external top_heap_words : unit -> int = "core_kernel_gc_top_heap_words" "noalloc"

external major_plus_minor_words : unit -> int = "core_kernel_gc_major_plus_minor_words"

let zero = int_of_string "0" (* The compiler won't optimize int_of_string away so it won't
                                perform constant folding below. *)
let rec keep_alive o =
  if zero <> 0 then keep_alive o

let%test_unit _ =
  let r = ref () in
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some r);
  Gc.compact ();
  assert (Option.is_some (Weak.get weak 0));
  keep_alive r
;;

module Expert = struct

  let add_finalizer x f =
    Caml.Gc.finalise
      (fun x -> Exn.handle_uncaught_and_exit (fun () -> f x))
      x
  ;;

  (* [add_finalizer_exn] is the same as [add_finalizer].  However, their types in
     core_gc.mli are different, and the type of [add_finalizer] guarantees that it always
     receives a heap block, which ensures that it will not raise, while
     [add_finalizer_exn] accepts any type, and so may raise. *)
  let add_finalizer_exn = add_finalizer

  let finalize_release = Caml.Gc.finalise_release

  module Alarm = struct
    type t = alarm

    let sexp_of_t _ = "<gc alarm>" |> [%sexp_of: string]

    let create f = create_alarm (fun () -> Exn.handle_uncaught_and_exit f)

    let delete = delete_alarm
  end
end

(* Simple inline benchmarks for GC functions *)
let%bench "minor_words" = minor_words ()
let%bench "major_words" = major_words ()
let%bench "major_plus_minor_words" = major_plus_minor_words ()
let%bench "promoted_words" = promoted_words ()
let%bench "minor_collections" = minor_collections ()
let%bench "major_collections" = major_collections ()
let%bench "heap_words" = heap_words ()
let%bench "heap_chunks" = heap_chunks ()
let%bench "compactions" = compactions ()
let%bench "top_heap_words" = top_heap_words ()
let%bench "stat" = stat ()
let%bench "quick_stat" = quick_stat ()
let%bench "counters" = counters ()
