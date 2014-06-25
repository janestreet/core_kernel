open Sexplib.Std
open Bin_prot.Std
include Caml.Gc

module Int = Core_int
module Sexp = Sexplib.Sexp
let sprintf = Printf.sprintf

module Stat = struct
  type pretty_float = float with compare, bin_io, sexp
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
    } with compare, bin_io, sexp, fields
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
      mutable minor_heap_size : int; (* The size (in words) of the minor heap. Changing this parameter will trigger a minor collection. Default: 32k. *)
      mutable major_heap_increment : int; (* The minimum number of words to add to the major heap when increasing it. Default: 62k. *)
      mutable space_overhead : int; (* The major GC speed is computed from this parameter. This is the memory that will be "wasted" because the GC does not immediatly collect unreachable blocks. It is expressed as a percentage of the memory used for live data. The GC will work more (use more CPU time and collect blocks more eagerly) if space_overhead is smaller. Default: 80. *)
      mutable verbose : int; (* This value controls the GC messages on standard error output. It is a sum of some of the following flags, to print messages on the corresponding events:
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
      mutable max_overhead : int; (* Heap compaction is triggered when the estimated amount of "wasted" memory is more than max_overhead percent of the amount of live data. If max_overhead is set to 0, heap compaction is triggered at the end of each major GC cycle (this setting is intended for testing purposes only). If max_overhead >= 1000000, compaction is never triggered. Default: 500. *)
      mutable stack_limit : int; (* The maximum size of the stack (in words). This is only relevant to the byte-code runtime, as the native code runtime uses the operating system's stack. Default: 256k. *)
      mutable allocation_policy : int; (** The policy used for allocating in the heap.  Possible values are 0 and 1.  0 is the next-fit policy, which is quite fast but can result in fragmentation.  1 is the first-fit policy, which can be slower in some cases but can be better for programs with fragmentation problems.  Default: 0. *)
    } with compare, bin_io, sexp, fields
  end

  include T
  include Comparable.Make(T)
end


let tune__field logger ?(fmt = ("%d" : (_, _, _) format)) name arg current =
  match arg with
  | None -> current
  | Some v ->
      Option.iter logger
        ~f:(fun f -> Printf.ksprintf f "Gc.Control.%s: %(%d%) -> %(%d%)"
              name fmt current fmt v);
      v
;;

(*
  *\(.*\) -> \1 = f "\1" \1 c.\1;
*)
let tune ?logger ?minor_heap_size ?major_heap_increment ?space_overhead
    ?verbose ?max_overhead ?stack_limit ?allocation_policy () =
  let c = get () in
  let f = tune__field logger in
  set {
    minor_heap_size = f "minor_heap_size" minor_heap_size c.minor_heap_size;
    major_heap_increment = f "major_heap_increment" major_heap_increment
      c.major_heap_increment;
    space_overhead = f "space_overhead" space_overhead c.space_overhead;
    verbose = f "verbose" ~fmt:"0x%x" verbose c.verbose;
    max_overhead = f "max_overhead" max_overhead c.max_overhead;
    stack_limit = f "stack_limit" stack_limit c.stack_limit;
    allocation_policy = f "allocation_policy" allocation_policy
      c.allocation_policy
  }
;;

external minor_words : unit -> int = "core_kernel_gc_minor_words" "noalloc"
external major_words : unit -> int = "core_kernel_gc_major_words" "noalloc"
external promoted_words : unit -> int = "core_kernel_gc_promoted_words" "noalloc"
external minor_collections : unit -> int = "core_kernel_gc_minor_collections" "noalloc"
external major_collections : unit -> int = "core_kernel_gc_major_collections" "noalloc"
external heap_words : unit -> int = "core_kernel_gc_heap_words" "noalloc"
external heap_chunks : unit -> int = "core_kernel_gc_heap_chunks" "noalloc"
external compactions : unit -> int = "core_kernel_gc_compactions" "noalloc"
external top_heap_words : unit -> int = "core_kernel_gc_top_heap_words" "noalloc"

let zero = int_of_string "0" (* The compiler won't optimize int_of_string away so it won't
                                perform constant folding below. *)
let rec keep_alive o =
  if zero <> 0 then keep_alive o

TEST_UNIT =
  let r = ref () in
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some r);
  Gc.compact ();
  assert (Option.is_some (Weak.get weak 0));
  keep_alive r;
;;

module Expert = struct
  let add_finalizer x f = Caml.Gc.finalise f x

  (* [add_finalizer_exn] is the same as [add_finalizer].  However, their types in
     core_gc.mli are different, and the type of [add_finalizer] guarantees that it always
     receives a heap block, which ensures that it will not raise, while
     [add_finalizer_exn] accepts any type, and so may raise. *)
  let add_finalizer_exn = add_finalizer

  let finalize_release = Caml.Gc.finalise_release
end

TEST_MODULE "gc" = struct

  (* The idea underlying this test is that minor_words does not allocate any memory. Hence
     the subsequent call to quick_stat should report exactly the same number. Also:

     1) This test may fail if the float is so large that it cannot fit in a 64bit int.

     2) We run this in a loop because the each call to [quick_stat] allocates minor_data
     and this number should be picked up by [minor_words] *)
  TEST_UNIT =
    for _i = 1 to 1000 do
      let mw1 = minor_words () in
      let st = quick_stat () in
      let mw2 = Float.iround_towards_zero_exn st.Stat.minor_words in
      assert (mw1 = mw2);
    done

  (* The point of doing a [minor] in the tests below is that [st] is still live and will
     be promoted during the minor GC, thereby changing both the promoted words and the
     major words in each iteration of the loop *)
  TEST_UNIT =
    for _i = 1 to 1000 do
      let mw1 = major_words () in
      let st = quick_stat () in
      minor ();
      let mw2 = Float.iround_towards_zero_exn st.Stat.major_words in
      assert (mw1 = mw2);
    done

  TEST_UNIT =
    for _i = 1 to 1000 do
      let mw1 = promoted_words () in
      let st = quick_stat () in
      minor ();
      let mw2 = Float.iround_towards_zero_exn st.Stat.promoted_words in
      assert (mw1 = mw2);
    done

  let stat_eq func projection =
    (* In the stub the record is allocated after getting the stats, so we must ensure
       [func] is called first. *)
    let x = func () in
    let y = projection (quick_stat ()) in
    x = y
  ;;

  TEST_UNIT =
    for _i = 1 to 1000 do
      assert (stat_eq minor_collections Stat.minor_collections);
      minor ();
      assert (stat_eq minor_collections Stat.minor_collections);
    done

  TEST_UNIT =
    for _i = 1 to 1000 do
      assert (stat_eq major_collections Stat.major_collections);
      major ();
      assert (stat_eq major_collections Stat.major_collections);
    done

  TEST_UNIT =
    for _i = 1 to 1000 do
      assert (stat_eq compactions Stat.compactions);
      compact ();
      assert (stat_eq compactions Stat.compactions);
    done

  TEST_UNIT =
    let check () =
      assert (stat_eq heap_chunks Stat.heap_chunks);
      assert (stat_eq heap_words Stat.heap_words);
      assert (stat_eq top_heap_words Stat.top_heap_words);
    in
    check ();
    let r = ref [] in
    let n = heap_chunks () in
    while not (heap_chunks () > n) do
      check ();
      r := String.create 128 :: !r
    done;
    check ()
end

(* Simple inline benchmarks for GC functions *)
BENCH "minor_words" = minor_words ()
BENCH "major_words" = major_words ()
BENCH "promoted_words" = promoted_words ()
BENCH "minor_collections" = minor_collections ()
BENCH "major_collections" = major_collections ()
BENCH "heap_words" = heap_words ()
BENCH "heap_chunks" = heap_chunks ()
BENCH "compactions" = compactions ()
BENCH "top_heap_words" = top_heap_words ()
BENCH "stat" = stat ()
BENCH "quick_stat" = quick_stat ()
BENCH "counters" = counters ()
