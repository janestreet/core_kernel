## 111.17.00

- In `Bigstring`, made many operations use compiler primitives new in
  OCaml 4.01.

  Exposed `Bigstring.get` and `set` as compiler primitives in the
  interface.

  Added `Bigstring.unsafe_get_int64_{le,be}_trunc`.
- Made `Error` round trip `exn`, i.e. `Error.to_exn (Error.of_exn exn)
  = exn`.
- Added to `failwiths` an optional `?here:Lexing.position` argument.
- Added `with typerep` to `Flags.S`.
- Optimized `List.dedup []` to return immediately.
- Added `data` argument to polymorphic type `Hashtbl_intf.Creators.create_options`.

  This allows implementations of `Hashtbl_intf.Creators` to have
  constructor arguments that depend on the type of both key and data
  values.  For example:

  ```ocaml
  module type Hashtbl_creators_with_typerep =
    Hashtbl_intf.Creators
    with type ('key, 'data, 'z) create_options
      =  typerep_of_key:'key Typerep.t
      -> typerep_of_data:'data Typerep.t
      -> 'z
  ```
- Improved the interface for getting `Monad.Make` to define `map` in
  terms of `bind`.

  Instead of passing a `map` function and requiring everyone who wants
  to define `map` using `bind` to call a special function, we use a
  variant type to allow the user to say what they want:

  ```ocaml
  val map : [ `Define_using_bind
            | `Custom of ('a t -> f:('a -> 'b) -> 'b t)
            ]
  ```
- Improved the performance of many `Dequeue` functions.

  Previously, many `Dequeue.dequeue`-type functions worked by raising
  and then catching an exception when the dequeue is empty.  This is
  much slower than just testing for emptiness, which is what the code
  now does.

  This improves the performance of `Async.Writer`, which uses
  `Dequeue.dequeue_front`.

## 111.13.00

- Added a `Sequence` module that implements polymorphic, on-demand
  sequences.

    Also implemented conversion to `Sequence.t` from various containers.

- Improved the explicitness and expressiveness of
  `Binary_searchable.binary_search`.

    `binary_search` now takes an additional (polymorphic variant)
    argument describing the relationship of the returned position to the
    element being searched for.

        val binary_search
          :  ?pos:int
          -> ?len:int
          -> t
          -> compare:(elt -> elt -> int)
          -> [ `Last_strictly_less_than         (** {v | < elt X |                       v} *)
             | `Last_less_than_or_equal_to      (** {v |      <= elt       X |           v} *)
             | `Last_equal_to                   (** {v           |   = elt X |           v} *)
             | `First_equal_to                  (** {v           | X = elt   |           v} *)
             | `First_greater_than_or_equal_to  (** {v           | X       >= elt      | v} *)
             | `First_strictly_greater_than     (** {v                       | X > elt | v} *)
             ]
          -> elt
          -> int option

- Added a new function, `Binary_searchable.binary_search_segmented`,
that can search an array consisting of two segments, rather than ordered
by `compare`.

        (** [binary_search_segmented ?pos ?len t ~segment_of which] takes an [segment_of]
            function that divides [t] into two (possibly empty) segments:

            {v
              | segment_of elt = `Left | segment_of elt = `Right |
            v}

            [binary_search_segmented] returns the index of the element on the boundary of the
            segments as specified by [which]: [`Last_on_left] yields the index of the last
            element of the left segment, while [`First_on_right] yields the index of the first
            element of the right segment.  It returns [None] if the segment is empty.

            By default, [binary_search] searches the entire [t].  One can supply [?pos] or
            [?len] to search a slice of [t].

            [binary_search_segmented] does not check that [segment_of] segments [t] as in the
            diagram, and behavior is unspecified if [segment_of] doesn't segment [t].  Behavior
            is also unspecified if [segment_of] mutates [t]. *)
        val binary_search_segmented
          :  ?pos:int
          -> ?len:int
          -> t
          -> segment_of:(elt -> [ `Left | `Right ])
          -> [ `Last_on_left | `First_on_right ]
          -> int option

- Made `Queue` match `Binary_searchable.S1`.
- Made `Gc.Stat` and `Gc.Control` match `Comparable`.
- Fixed some unit tests in `Type_immediacy` that were fragile due to GC.

## 111.11.00

- Added to `String` functions for substring search and replace, based
  on the KMP algorithm.

  Here are some benchmarks, comparing `Re2` for a fixed pattern,
  Mark's kmp from extended_string, and this implementation ("needle").

  The pattern is the usual `abacabadabacabae...`.  The text looks
  similar, with the pattern occurring at the very end.

  For =Re2= and =Needle= search benchmarks, the pattern is
  preprocessed in advance, outside of the benchmark.

  FWIW: I've also tried searches with pattern size = 32767, but =Re2=
  blows up, saying:

  ```
  re2/dfa.cc:447: DFA out of memory: prog size 32771 mem 2664898
  ```

  | Name                          |        Time/Run |       mWd/Run |    mjWd/Run | Prom/Run | Percentage |
  |-------------------------------|-----------------|---------------|-------------|----------|------------|
  | create_needle_15              |        102.56ns |        21.00w |             |          |            |
  | re2_compile_15                |      6_261.48ns |               |       3.00w |          |      0.01% |
  | create_needle_1023            |     13_870.48ns |         5.00w |   1_024.01w |          |      0.03% |
  | re2_compile_1023              |    107_533.32ns |               |       3.03w |          |      0.24% |
  | create_needle_8191            |     90_107.02ns |         5.00w |   8_192.01w |          |      0.20% |
  | re2_compile_8191              |  1_059_873.47ns |               |       3.28w |    0.28w |      2.37% |
  | create_needle_524287          |  6_430_623.96ns |         5.00w | 524_288.09w |          |     14.35% |
  | re2_compile_524287            | 44_799_605.83ns |               |       3.77w |    0.77w |    100.00% |
  | needle_search_15_95           |        349.65ns |         4.00w |             |          |            |
  | re2_search_15_95              |        483.11ns |               |             |          |            |
  | mshinwell_search_15_95        |      1_151.38ns |       781.01w |             |          |            |
  | needle_search_15_815          |      2_838.85ns |         4.00w |             |          |            |
  | re2_search_15_815             |      3_293.06ns |               |             |          |            |
  | mshinwell_search_15_815       |      8_360.57ns |     5_821.07w |       0.55w |    0.55w |      0.02% |
  | needle_search_15_2415         |      8_395.84ns |         4.00w |             |          |      0.02% |
  | re2_search_15_2415            |      9_594.14ns |               |             |          |      0.02% |
  | mshinwell_search_15_2415      |     24_602.09ns |    17_021.16w |       1.62w |    1.62w |      0.05% |
  | needle_search_1023_6143       |     14_825.50ns |         4.00w |             |          |      0.03% |
  | re2_search_1023_6143          |     40_926.59ns |               |             |          |      0.09% |
  | mshinwell_search_1023_6143    |     81_930.46ns |    49_149.66w |   1_025.65w |    1.65w |      0.18% |
  | needle_search_1023_52223      |    126_465.96ns |         4.00w |             |          |      0.28% |
  | re2_search_1023_52223         |    365_359.98ns |               |             |          |      0.82% |
  | mshinwell_search_1023_52223   |    527_323.73ns |   371_715.39w |   1_033.17w |    9.17w |      1.18% |
  | needle_search_1023_154623     |    377_539.53ns |         4.00w |             |          |      0.84% |
  | re2_search_1023_154623        |  1_001_251.93ns |               |             |          |      2.23% |
  | mshinwell_search_1023_154623  |  1_499_835.01ns | 1_088_518.15w |   1_033.19w |    9.19w |      3.35% |
  | needle_search_8191_49151      |    115_223.31ns |         4.00w |             |          |      0.26% |
  | re2_search_8191_49151         |    559_487.38ns |               |             |          |      1.25% |
  | mshinwell_search_8191_49151   |    653_981.19ns |   393_219.50w |   8_201.01w |    9.01w |      1.46% |
  | needle_search_8191_417791     |    976_725.24ns |         4.00w |             |          |      2.18% |
  | re2_search_8191_417791        |  4_713_965.69ns |               |             |          |     10.52% |
  | mshinwell_search_8191_417791  |  4_224_417.93ns | 2_973_709.32w |   8_202.37w |   10.37w |      9.43% |
  | needle_search_8191_1236991    |  2_912_863.78ns |         4.00w |             |          |      6.50% |
  | re2_search_8191_1236991       | 14_039_230.59ns |               |             |          |     31.34% |
  | mshinwell_search_8191_1236991 | 11_997_713.73ns | 8_708_130.87w |   8_202.47w |   10.47w |     26.78% |
- Added to `Set` functions for converting to and from a `Map.t`.

  ```ocaml
  val to_map : ('key, 'cmp) t -> f:('key -> 'data) -> ('key, 'data, 'cmp) Map.t
  val of_map_keys : ('key, _, 'cmp) Map.t -> ('key, 'cmp) t
  ```

  This required adding some additional type trickery to
  `Core_set_intf` to indicate that the comparator for a given module
  may or may not be fixed.
- Added an optional `iter` parameter to `Container.Make`.

  A direct implementation of `iter` is often more efficient than
  defining `iter` in terms of `fold`, and in these cases, the results
  of `Container.Make` that are defined in terms of `iter` will be more
  efficient also.
- Added `Int.pow` (and for other integer types), for bounds-checked
  integer exponentiation.

## 111.08.00

- Added `Hashtbl.for_all` and `for_alli`.
- Added `Float.to_padded_compact_string` for converting a floating point
  number to a lossy, compact, human-readable representation.

    E.g., `1_000_001.00` becomes `"1m "`.

- Tweaked the form of the definition of `Blang.Stable.V1`.

    Removed a `type t_` that is not necessary now that we can use `nonrec`
    without triggering spurious warnings.

## 111.06.00

- Added inline benchmarks for `Array`

  Here are some of the results from the new benchmarks, with some
  indexed tests dropped.

  | Name                                                |    Time/Run | mWd/Run |  mjWd/Run |
  |-----------------------------------------------------|-------------|---------|-----------|
  | [core_array.ml:Alloc] create:0                      |     13.65ns |         |           |
  | [core_array.ml:Alloc] create:100                    |     99.83ns | 101.00w |           |
  | [core_array.ml:Alloc] create:255                    |    201.32ns | 256.00w |           |
  | [core_array.ml:Alloc] create:256                    |  1_432.43ns |         |   257.00w |
  | [core_array.ml:Alloc] create:1000                   |  5_605.58ns |         | 1_001.01w |
  | [core_array.ml:Blit.Poly] blit (tuple):10           |     87.10ns |         |           |
  | [core_array.ml:Blit.Poly] blito (tuple):10          |    112.14ns |   2.00w |           |
  | [core_array.ml:Blit.Poly] blit (int):10             |     85.25ns |         |           |
  | [core_array.ml:Blit.Poly] blito (int):10            |    107.23ns |   2.00w |           |
  | [core_array.ml:Blit.Poly] blit (float):10           |     84.71ns |         |           |
  | [core_array.ml:Blit.Poly] blito (float):10          |     86.71ns |   2.00w |           |
  | [core_array.ml:Blit.Int] blit:10                    |     19.77ns |         |           |
  | [core_array.ml:Blit.Int] blito:10                   |     23.54ns |   2.00w |           |
  | [core_array.ml:Blit.Float] blit:10                  |     19.87ns |         |           |
  | [core_array.ml:Blit.Float] blito:10                 |     24.12ns |   2.00w |           |
  | [core_array.ml:Is empty] Polymorphic '='            |     18.21ns |         |           |
  | [core_array.ml:Is empty] Array.equal                |      8.08ns |   6.00w |           |
  | [core_array.ml:Is empty] phys_equal                 |      2.98ns |         |           |
  | [core_array.ml:Is empty] Array.is_empty (empty)     |      2.98ns |         |           |
  | [core_array.ml:Is empty] Array.is_empty (non-empty) |      3.00ns |         |           |
- Moved `Thread_safe_queue` to core
- Generalized the type of `Exn.handle_uncaught_and_exit` to `(unit ->
  'a) -> 'a`.

  In the case where `handle_uncaught_and_exit` succeeds, it can return
  the value of the supplied function.

  It's type had been:

  ```ocaml
  val handle_uncaught_and_exit : (unit -> never_returns) -> never_returns
  ```
- Added `Int.round*` functions for rounding to a multiple of another
  int.

  ```ocaml
  val round : ?dir:[ `Zero | `Nearest | `Up | `Down ] -> t -> to_multiple_of:t -> t

  val round_towards_zero : t -> to_multiple_of:t -> t
  val round_down         : t -> to_multiple_of:t -> t
  val round_up           : t -> to_multiple_of:t -> t
  val round_nearest      : t -> to_multiple_of:t -> t
  ```

  These functions were added to `Int_intf.S`, implemented by `Int`,
  `Nativeint`, `Int32`, and `Int64`.

  Various int modules were also lightly refactored to make it easier
  in the future to implement common operators available for all
  modules implementing the int interface via a functor to share the
  code.

## 111.03.00

- Added `Error.to_string_hum_deprecated` that is the same as
  `Error.to_string_hum` pre 109.61.
- Changed `Error.to_string_hum` so that
  `Error.to_string_hum (Error.of_string s) = s`.

  This fixed undesirable sexp escaping introduced in 109.61 and
  restores the pre-109.61 behavior for the special case of
  `Error.of_string`.  A consequence of the removal of the custom
  `to_string_hum` converter in 109.61 was that:

  ```ocaml
  Error.to_string_hum (Error.of_string s) =
      Sexp.to_string_hum (Sexp.Atom s)
  ```

  That introduced sexp escaping of `s`.
- Added to `Doubly_linked` functions for moving an element
  within a list.

  ```ocaml
  val move_to_front : 'a t -> 'a Elt.t -> unit
  val move_to_back  : 'a t -> 'a Elt.t -> unit
  val move_after    : 'a t -> 'a Elt.t -> anchor:'a Elt.t -> unit
  val move_before   : 'a t -> 'a Elt.t -> anchor:'a Elt.t -> unit
  ```
- Improved `Core_map_unit_tests.Unit_tests` to allow arbitrary data
  in the map, not just `ints`.

  This was done by eta expansion.

## 110.01.00

- Changed `Queue` from a linked to an array-backed implementation.

  Renamed the previous implementation to `Linked_queue`.

  Renamed `transfer`, which was constant time, as `blit_transfer`,
  which is linear time.

  Removed `partial_iter`.  One can use `with_return`.

  Added `singleton`, `filter`, `get`, `set`.
- For `Error` and `Info`, changed `to_string_hum` to use `sexp_of_t`
  and `Sexp.to_string_hum`, rather than a custom string format.
- Changed the output format of `Validate.errors` to be a sexp.
- Added `Hashtbl.of_alist_or_error` and `Map.of_alist_or_error`.
- Added `String_id.Make` functor, which includes a module name for
  better error messages.
- Exposed `Bucket.size`.
- Changed the default for `Debug.should_print_backtrace` to be `false`
  rather than `true`.

  Usually the backtraces are noise.
- Removed the tuning of gc parameters built in to Core, so that the
  default is now the stock OCaml settings.

  Such tuning doesn't belong in Core, but rather done per application.
  Also, the Core settings had fallen way out of date, and not kept up
  with changes in the OCaml runtime settings.  We have one example
  (lwt on async) where the Core settings significantly slowed down a
  program.
- Added `Exn.raise_without_backtrace`, to raise without building a
  backtrace.

  `raise_without_backtrace` never builds a backtrace, even when
  `Backtrace.am_recording ()`.
- Made `with_return` faster by using `Exn.raise_without_backtrace`.
- Improved `with_return` to detect usage of a `return` after its
  creating `with_return` has returned.

## 109.60.00

- Added `Gc.keep_alive`, which ensures its argument is live at the point
  of the call.
- Added `Sexp.With_text` module, which keeps a value and the a sexp it
  was generated from, preserving the original formatting.

## 109.58.00

- Moved all of the `Gc` module into `Core_kernel`.

  Part of the `Gc` module used to be in `Core` because it used
  threads.  But it doesn't use threads anymore, so can be all in
  `Core_kernel`.
- Made `Stable.Map` and `Set` have `with compare`.
- Added `String.rev`.

  Closes janestreet/core#16

  We will not add `String.rev_inplace`, as we do not want to encourage
  mutation of strings.
- Made `Univ_map.Key` equivalent to `Type_equal.Id`.
- Added `Univ.view`, which exposes `Univ.t` as an existential, `type t
  = T : 'a Id.t * 'a -> t`.

  Exposing the existential makes it possible to, for example, use
  `Univ_map.set` to construct a `Univ_map.t`from a list of `Univ.t`s.

  This representation is currently the same as the underlying
  representation, but to make changes to the underlying representation
  easier, it has been put in a module `Univ.View`.

## 109.55.00

- Added `with typerep` to many `Core` types.
- Changed `Flat_queue` to raise if the queue is mutated during
  iteration.
- Improved `Map.merge` to run in linear time.

## 109.53.00

- Added `Float.to_string_round_trippable`, which produces a string
  that loses no precision but (usually) uses as few digits as
  possible.

  This can eliminate noise at the end (e.g. `3.14` not
  `3.1400000000000001243`).

  Benchmarks:

  New sexp:

  | Name                   | Time/Run | mWd/Run | Percentage |
  |------------------------|----------|---------|------------|
  | new Float.sexp_of 3.14 | 463.28ns |   6.00w |     48.88% |
  | new Float.sexp_of e    | 947.71ns |  12.00w |    100.00% |

  Old sexp:

  | Name                   | Time/Run | mWd/Run | Percentage |
  |------------------------|----------|---------|------------|
  | old Float.sexp_of 3.14 | 841.99ns | 178.00w |     98.03% |
  | old Float.sexp_of e    | 858.94ns | 178.00w |    100.00% |

  Much of the speedup in the 3.14 case comes from the fact that
  `format_float "%.15g"` is much faster than `sprintf "%.15g"`.  And
  of course the above does not capture any of the benefits of dealing
  with shorter strings down the road.

  Here are some detailed benchmarks of the various bits and pieces of
  what's going on here:

  | Name                                |   Time/Run | mWd/Run | Percentage |
  |-------------------------------------|------------|---------|------------|
  | format_float '%.15g' 3.14           |   335.96ns |   2.00w |     32.71% |
  | format_float '%.17g' 3.14           |   394.18ns |   4.00w |     38.38% |
  | format_float '%.20g' 3.14           |   459.79ns |   4.00w |     44.77% |
  | format_float '%.40g' 3.14           |   638.06ns |   7.00w |     62.13% |
  | sprintf '%.15g' 3.14                |   723.71ns | 165.00w |     70.47% |
  | sprintf '%.17g' 3.14                |   803.44ns | 173.00w |     78.23% |
  | sprintf '%.20g' 3.14                |   920.78ns | 176.00w |     89.66% |
  | sprintf '%.40g' 3.14                |   990.09ns | 187.00w |     96.41% |
  | format_float '%.15g' e              |   357.59ns |   4.00w |     34.82% |
  | format_float '%.17g' e              |   372.16ns |   4.00w |     36.24% |
  | format_float '%.20g' e              |   434.59ns |   4.00w |     42.32% |
  | format_float '%.40g' e              |   592.78ns |   7.00w |     57.72% |
  | sprintf '%.15g' e                   |   742.12ns | 173.00w |     72.26% |
  | sprintf '%.17g' e                   |   747.92ns | 173.00w |     72.83% |
  | sprintf '%.20g' e                   |   836.30ns | 176.00w |     81.43% |
  | sprintf '%.40g' e                   | 1_026.96ns | 187.00w |    100.00% |
  | valid_float_lexem 12345678901234567 |    76.29ns |   9.00w |      7.43% |
  | valid_float_lexem 3.14              |     9.28ns |   5.00w |      0.90% |
  | float_of_string 3.14                |   130.19ns |   2.00w |     12.68% |
  | float_of_string 1234567890123456.7  |   184.33ns |   2.00w |     17.95% |
  | to_string 3.14                      |   316.47ns |   7.00w |     30.82% |
  | to_string_round_trippable 3.14      |   466.02ns |   9.00w |     45.38% |
  | to_string e                         |   315.41ns |   7.00w |     30.71% |
  | to_string_round_trippable e         |   949.12ns |  15.00w |     92.42% |

- Replaced `Float.min_positive_value` with `min_positive_normal_value`
  and `min_positive_subnormal_value`.
- Added some functions to `Float.O`: `abs`, `of_float`, and
  `Robustly_comparable.S`.
- Small improvements to the `Heap` module.

  Implemented `Heap.iter` directly rather than in terms of `fold`.

  In `heap.ml`, fixed the idiom for using `Container.Make`.
- Added an `Int.O` and other `Int*.O` modules, with arithmetic
  operators, infix comparators, and a few useful arithmetic values.
- Added `Int.( ~- )`, for unary negation.
- Added `Pool.unsafe_free`.
- Added `Percent` module.

## 109.52.00

- Added to `Binary_packing` module functions for packing and unpacking
  signed 64-bit ints in little- and big-endian.
- Changed the `Comparator` interfaces to no longer have `with bin_io`
  or `with sexp`.

  The `Comparator` interfaces are now just about having a comparator.

  Also, renamed `type comparator` as `type comparator_witness`.  And,
  removed `Comparator.S_binable`, since one can use:

  ```ocaml
  type t with bin_io
  include Comparator.S with type t :` t
  ```
- Changed `Comparator.Make` to return a module without a type `t`,
  like other `*able` functors,

   This made it possible to remove the signature constraint when
  `Comparator.Make` is applied.
- Made `Comparable.S_binable` be like `Comparable.S` and not have
  `type t with sexp`.

  The following two functors now fail to type check:

  ```ocaml
  module F1 (M : Comparable.S        ) : sig type t with sexp end ` M
  module F2 (M : Comparable.S_binable) : sig type t with sexp end ` M
  ```

  whereas previously `F1` was rejected and `F2` was accepted.
- Changed the `Monad.Make` functor to require a `val map` argument.

  This was done since we almost always want a specialized `map`, and
  we kept making the mistake of not overriding the generic one in the
  three places needed.

  Added `Monad.map_via_bind`, which one can use to create a standard
  `map` function using `bind` and `return`.
- Removed unnecessary signature constraints on the result of applying
  `Monad.Make`.

  Some time ago, `Monad.Make` changed from returning:

  ```ocaml
  S with type 'a t ` 'a M.t
  ```

  to returning:

  ```ocaml
  S with type 'a t :` 'a M.t
  ```

  so we no longer need to constrain the result of `Monad.Make` at its
  uses to remove `t`.
- Changed `String.exists` and `String.for_all` to iterate by
  increasing index rather than decreasing.
- Added `with compare` to module `Ref`.
- Made `Flags` be `Comparable`, with the order consistent with bitwise
  subset.
- Cleaned up the implementation of `Union_find`.

  Improvemed the code in `union_find.ml`:

  * Removed an assert false.
  * do not reallocate a parent node during compress. This should
    result in more stability for sets memory wise.
  * Added implementation notes.
  * Renamed internal variant constructors.
  * Added unit tests.
- Added `Float.O`, a sub-module intended to be used with local opens.

  The idea is to be able to write expressions like:

  ```ocaml
  Float.O.((3. + 4.) > 6. / 2.)
  ```

  This idiom is expected to be extended to other modules as well.
- Added a `sexp_of_t` converter to `Type_equal.Id`.
- Replaced `Univ.Constr` with `Type_equal.Id`.
- Added `Debug.eprintf`, analogous to `eprint` and `eprints`.

## 109.47.00

- Added `Error.to_info` and `of_info`.
- Significantly sped up `Float.iround_*` functions.

  For `iround_down_exn`, the new version appears to use about 25% of the
  CPU time of the old version on non-negative floats.  For negative
  floats it uses around 60% of the CPU time.

  | Name                    | Time (ns) | % of max |
  |-------------------------|-----------|----------|
  | old iround_down_exn pos |     15.02 |    95.23 |
  | new iround_down_exn pos |      3.75 |    23.75 |
  | old iround_down_exn neg |     15.78 |   100.00 |
  | new iround_down_exn neg |      9.80 |    62.10 |
- Added `Binary_searchable.Make` functor to core, and used it in `Array` and `Dequeue`.
- Fixed `Bounded_int_table` to match `Invariant.S2`.
- Added to `Pool` support for `10-`, `11-`, and `12-` tuples.
- Added functions to the `Gc` module to get usage information without allocating.

  Added these functions, all of type `unit -> int`:

  ```
  minor_collections
  major_collections
  heap_words
  heap_chunks
  compactions
  top_heap_words
  ```

  They all satisfy:

  ```ocaml
  Gc.f () = (Gc.quick_stat ()).Gc.Stat.f
  ```

  They all avoid the allocation of the stat record, so one can monitor
  the garbage collector without perturbing it.

## 109.45.00

- Changed `Blang.bind` to short-circuit `And`, `Or`, and `If`
  expressions.

  For example if `bind t1 f ` false`, then `bind (and_ t1 t2) `
  false`, and will not evaluate `bind t2 f`.

- Renamed `Dequeue.get` as `get_opt`, and `get_exn` as `get`, to be
  consistent with other containers which don't use the `_exn` suffix
  for subscripting exceptions.
- Removed `Source_code_position.to_sexp_hum`, in favor of
  `sexp_of_t_hum`, which works smoothly with `with sexp`.
- Changed `Flat_queue_unit_tests` to run `Flat_queue.invariant`, which
  was mistakenly not being used.

## 109.44.00

- Implemented `Dequeue.iter` directly, instead of as a specialization
  of `fold`.

  Extended random tests to cover `iter`.

## 109.42.00

- Added `Array.is_sorted_strictly` and `List.is_sorted_strictly`.

  ```ocaml
  val is_sorted_strictly : 'a t -> cmp:('a -> 'a -> int) -> bool
  ```

- Added `Array.find_consecutive_duplicate` and `List.find_consecutive_duplicate`.

  ```ocaml
  val find_consecutive_duplicate : 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option
  ```

- Added `Array.truncate`, which changes (shortens) the length of an array.

  ```ocaml
  val truncate : _ t -> len:int -> unit
  ```

- Improved the debugging message in `Bounded_int_table.remove` to show the data structure's details.

- Added `Float.iround_lbound` and `iround_ubound`, the bounds for rounding to `int`.

- Added `Hashtbl.similar`, which is like `equal`, but allows the types of the values in the two tables to differ.

- Added `Pool.Pointer.phys_compare`, which is analagous to `phys_equal`, and does not require an argument comparison function.

  ```ocaml
  val phys_compare : 'a t -> 'a t -> int
  ```
- Exposed that `Pool.Debug`'s output types are the same as its input types.

## 109.41.00

- Added `Map.of_alist_reduce`.

  This function is a natural addition alongside `of_alist_fold`.  Its
  advantage is that it does not require an `init` argument like
  `of_alist_fold`.  Moreover, it does not involve `option` types, like
  `List.reduce` does in order to handle the empty list case.

## 109.39.00

- Implemented `Heap.iter` directly instead of in terms of `fold`.

## 109.37.00

- Added Core.Std.Poly as a short name for
  Core.Std.Polymorphic_compare.
- Exposed module Core.Std.Decimal.

## 109.36.00

- Made `Hashtbl.Poly.hash` equal `Caml.Hashtbl.hash`, and changed changed `String.hash` and `Float.hash` to match OCaml's hash function.

  Previously, `Core.Poly.hash` had been defined as:

  ```ocaml
  let hash x = hash_param 10 100 x
  ```

  This fell out of sync with OCaml's hash function, and was providing worse hash values.

- Fixed `Obj_array.singleton` to never create a float array.

  Also made it clearer that `Obj_array.copy` could never create a float
  array.

- Changed `Pool.create` to allow zero-length pools.

  Previously, `Pool.create ~capacity:0` had raised, which made it easy
  to write code that blows up on edge cases for no apparent reason.  For
  example, `Heap.copy` was written in a way that copying an empty heap
  would blow up (regardless of its capacity), and `Heap.of_array` would
  also blow up on an empty array.

- Added `String.split_lines`.

  ```ocaml
  (** [split_lines t] returns the list of lines that comprise [t].  The lines do
      not include the trailing ["\n"] or ["\r\n"]. *)
  val split_lines : t -> t list
  ```

## 109.35.00

- Added `with compare` to `List.Assoc.t`.
- Made `Pooled_hashtbl.create` handle non-positive and very large
  `size`s in the same way as `Core.Hashtbl`.
- Added `is_error`, `is_ok`, and `does_raise` to `Core.Std`.

  ```ocaml
  let is_error ` Result.is_error
  let is_ok    ` Result.is_ok
  val does_raise : (unit -> _) -> bool
  ```
- Reimplemented `Heap` and reworked the interface to be more standard.

  The new implementation uses pairing heaps and `Pool`.
- Added a module `Pool.Unsafe`, which is like `Pool`, except that
  `create` doesn't require an initial value.

  This makes it unsafe to access pool pointers after they have been
  freed.  But it is useful for situations when one isn't able to
  create an initial value, e.g. `Core.Heap`.
- Removed `Time.to_localized_string` and `Time.to_string_deprecated`.

  These did not include the time-zone offset.  Instead, use
  `Time.to_string` and `Time.to_string_abs`, which do include the
  time-zone offset.
- Exposed that `Int63.t = private int` on 64-bit machines.

  This lets the OCaml compiler avoid `caml_modify` when dealing with
  it.
- Added `Gc` stat functions that don't allocate: `Gc.minor_words`,
  `Gc.major_words`, `Gc.promoted_words`.

  Added the following `Gc` functions:

  ```ocaml
  Gc.minor_words : unit -> int
  Gc.major_words : unit -> int
  Gc.promoted_words : unit -> int
  ```

  such that these functions cause no allocations by themselves. The
  assumption being that 63-bit ints should be large enough to express
  total allocations for most programs.  On 32-bit machines the numbers
  may overflow and these functions are not as generally useful.

  These functions were added because doing memory allocation debugging
  with `Gc.quick_stat` as the primary means of understanding
  allocations is difficult: tracking down allocations of the order of
  a few hundred words in a hot loop by putting in lots of `quick_stat`
  statements becomes too intrusive because of the memory allocations
  they cause.

  Here are some benchmarks of existing `Gc` functions and the newly
  added functions:

  ```
  $ ./test_bench.exe -q 2 -clear name time +alloc +time-err
  Estimated testing time 12s (change using -quota SECS).
  ```

  | Name            | Time (ns) |      95% ci | Time R^2 | Minor |
  |-----------------|-----------|-------------|----------|-------|
  | quick_stat      |     92.16 | +0.72 -0.64 |     1.00 | 23.00 |
  | counters        |     33.63 | +0.26 -0.23 |     1.00 | 10.00 |
  | allocated_bytes |     37.89 | +0.34 -0.32 |     1.00 | 12.00 |
  | minor_words     |      4.63 | +0.03 -0.02 |     1.00 |       |
  | major_words     |      4.36 | +0.02 -0.02 |     1.00 |       |
  | promoted_words  |      4.10 | +0.03 -0.02 |     1.00 |       |

## 109.34.00

- Added a new module, `Flat_queue`, which is a queue of flat tuples.

  This is essentially:

  ```ocaml
  ('a1 * .. * 'aN) Queue.t
  ```

  However the queue is implemented as a `Flat_array`, so the tuples are layed out
  flat in the array and not allocated.

- Improved `Bounded_int_table.remove`'s error message when it detects an internal inconsistency.

- Added new `Debug` module.

- Changed `Invariant.invariant` to take `_here_` rather than a string.

- Made `Float` satisfy the `Identifiable` interface.

## 109.32.00

- Added `val Option.merge: 'a t -> 'a t -> f:('a -> 'a -> 'a) -> 'a t`.

- Added `val Validate.failf : ('a, unit, string, t) format4 -> 'a`.

- In `Validated.Make_binable`, made it possible to apply the validation function when un-bin-io-ing a value.

- Added `module Pooled_hashtbl` to `module type Hashable`.

  This is an alternative implementation to `Core.Hashtbl`.  It uses a
  standard linked list to resolve hash collisions, and `Pool` to manage
  the linked-list nodes.

## 109.31.00

- Renamed some functions in module `Lazy`: dropped the `lazy_` prefix from `is_val`, `from_val`, and `from_fun`.

## 109.30.00

  - Added module, `Core.Blit`, which codifies the type, implementation, and unit-testing of blit functions.

  - Added `remove_zero_flags` option to `Flags.Make`, to support flags that are zero.

    This fixes a problem with `Flags.Make` on CentOS 5 because `O_CLOEXEC` is `0` there.

  - Removed `Pool.None`, and folded `Pool.Obj_array` into `Pool` proper.

    `Pool.None` had its day, but `Pool.Obj_array` dominates it, so we don't need it any more.

## 109.28.00

- Moved all the contents of the `Zero` library into `Core`, mostly
  into `Core_kernel`.

  We want to start using `Zero` stuff more in `Core`, which couldn't
  be done with `Zero` as a separate library.

  Everything moved into `Core_kernel`, except for `Timing_wheel`,
  which moved into `Core` proper, due to its dependence on `Time`.
- Renamed `Flat_tuple_array` as `Flat_array`.
- Added `Dequeue.{front,back}_index_exn`

  These are more efficient than using `{front,back}_index` and then
  `Option.value_exn`.
- Exposed `Core.String.unsafe_{get,set}`.

