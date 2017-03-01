(** [Std_kernel] defines modules exposed by [Core_kernel] that are not overridden by
    [Core].  It is used in [core.ml] to re-export these modules. *)

open! Import

include Std_internal

module Applicative                          = Applicative
module Arg                                  = Core_arg
module Avltree                              = Avltree
module Backtrace                            = Backtrace
module Bag                                  = Bag
module Bigsubstring                         = Bigsubstring
module Bin_prot                             = Core_bin_prot
module Binable                              = Binable
module Binary_packing                       = Binary_packing
module Binary_search                        = Binary_search
module Binary_searchable                    = Binary_searchable
module Blang                                = Blang
module Blit                                 = Blit
module Bounded_index                        = Bounded_index
module Bounded_int_table                    = Bounded_int_table
module Bucket                               = Bucket
module Bus                                  = Bus
module Bytes                                = Core_bytes
module Byte_units                           = Byte_units
module Commutative_group                    = Commutative_group
module Comparable                           = Comparable
module Container                            = Container
module Day_of_week                          = Day_of_week
module Debug                                = Debug
module Deque                                = Deque
module Deriving_hash                        = Deriving_hash
module Doubly_linked                        = Doubly_linked
module Either                               = Either
module Ephemeron                            = Core_ephemeron
module Equal                                = Equal
module Error                                = Error
module Expect_test_config                   = Expect_test_config
module Fdeque                               = Fdeque
module Fheap                                = Fheap
module Flags                                = Flags
module Float                                = Float
module Float_with_finite_only_serialization = Float_with_finite_only_serialization
module Floatable                            = Floatable
module Fn                                   = Fn
module Force_once                           = Force_once
module Fqueue                               = Fqueue
module Gc                                   = Core_gc
module Hash                                 = Hash
module Hash_heap                            = Hash_heap
module Hash_queue                           = Hash_queue
module Hash_set                             = Hash_set
module Hashable                             = Hashable
module Hashtbl_intf                         = Core_hashtbl_intf
module Heap                                 = Heap
module Heap_block                           = Heap_block
module Hexdump                              = Hexdump
module Host_and_port                        = Host_and_port
module Identifiable                         = Identifiable
module Immediate_option                     = Immediate_option
module In_channel                           = In_channel
module Info                                 = Info
module Int_conversions                      = Base.Not_exposed_properly.Int_conversions
module Int_intf                             = Int_intf
module Int_set                              = Int_set
module Interfaces                           = Interfaces
module Invariant                            = Invariant
module Limiter                              = Limiter
module Linked_stack                         = Linked_stack
module Maybe_bound                          = Maybe_bound
module Memo                                 = Memo
module Monad                                = Monad
module Month                                = Month
module Moption                              = Moption
module No_polymorphic_compare               = No_polymorphic_compare
module Nothing                              = Nothing
module Only_in_test                         = Only_in_test
module Option                               = Option
module Option_array                         = Option_array
module Ordered_collection_common            = Ordered_collection_common
module Or_error                             = Or_error
module Out_channel                          = Out_channel
module Percent                              = Percent
module Pid                                  = Pid
module Poly                                 = Poly
module Polymorphic_compare                  = Polymorphic_compare
module Pool                                 = Pool
module Pooled_hashtbl                       = Pooled_hashtbl
module Pretty_printer                       = Pretty_printer
module Printexc                             = Core_printexc
module Printf                               = Core_printf
module Queue                                = Core_queue
module Quickcheck                           = Quickcheck
module Quickcheckable                       = Quickcheckable
module Robustly_comparable                  = Robustly_comparable
module Rope                                 = Rope
module Sequence                             = Sequence
module Set_once                             = Set_once
module Sexp_maybe                           = Core_sexp.Sexp_maybe
module Sexpable                             = Sexpable
module Sign                                 = Sign
module Source_code_position                 = Source_code_position
module Splittable_random                    = Splittable_random
module Stable_unit_test                     = Stable_unit_test
module Stack                                = Core_stack
module String_id                            = String_id
module Stringable                           = Stringable
module Substring                            = Substring
module Substring_intf                       = Substring_intf
module Thread_safe_queue                    = Thread_safe_queue
module Timing_wheel_ns                      = Timing_wheel_ns
module Total_map                            = Total_map
module Tuple                                = Tuple
module Tuple2                               = Tuple.T2
module Tuple3                               = Tuple.T3
module Type_immediacy                       = Type_immediacy
module Uniform_array                        = Uniform_array
module Union_find                           = Union_find
module Unique_id                            = Unique_id
module Unit                                 = Unit
module Unit_of_time                         = Unit_of_time
module Univ                                 = Univ
module Univ_map                             = Univ_map
module Unpack_buffer                        = Unpack_buffer
module Validate                             = Validate
module Validated                            = Validated
module Weak                                 = Core_weak
module Weak_pointer                         = Weak_pointer
module With_return                          = With_return
module Word_size                            = Word_size

module type Unique_id = Unique_id.Id

include T

type 'a _maybe_bound = 'a Maybe_bound.t =
    Incl of 'a | Excl of 'a | Unbounded

(* Some people have proposed removing [does_raise], but there isn't consensus. *)
let does_raise = Exn.does_raise

type bytes =
  [ `This_type_does_not_equal_string_because_we_want_type_errors_to_say_string ]
;;

(* We perform these side effects here because we want them to run for any code that uses
   [Core_kernel].  If this were in another module in [Core_kernel] that was not used in
   some program, then the side effects might not be run in that program.  This will run as
   long as the program refers to at least one value directly in [Std_kernel]; referring to
   values in [Std_kernel.Bool], for example, is not sufficient. *)
let () =
  Exn.initialize_module ();
;;

let am_running_inline_test = Ppx_inline_test_lib.Runtime.am_running_inline_test

let sec = Time_float.Span.of_sec
