(* Modules that are not overridden by Core.Std *)

include Std_internal

module Applicative               = Applicative
module Arg                       = Core_arg
module Avltree                   = Avltree
module Backtrace                 = Backtrace
module Bag                       = Bag
module Bigsubstring              = Bigsubstring
module Bin_prot                  = Core_bin_prot
module Binable                   = Binable
module Binary_packing            = Binary_packing
module Binary_searchable         = Binary_searchable
module Blang                     = Blang
module Blit                      = Blit
module Bounded_int_table         = Bounded_int_table
module Bucket                    = Bucket
module Bus                       = Bus
module Bytes                     = Core_bytes
module Byte_units                = Byte_units
module Commutative_group         = Commutative_group
module Comparable                = Comparable
module Comparator                = Comparator
module Container                 = Container
module Day_of_week               = Day_of_week
module Decimal                   = Decimal
module Debug                     = Debug
module Deque                     = Deque
module Dequeue                   = Dequeue
module Doubly_linked             = Doubly_linked
module Either                    = Either
module Equal                     = Equal
module Error                     = Error
module Exn                       = Exn
module Fdeque                    = Fdeque
module Fheap                     = Fheap
module Flags                     = Flags
module Flat_array                = Flat_array
module Flat_queue                = Flat_queue
module Float                     = Float
module Float_intf                = Float_intf
module Floatable                 = Floatable
module Fn                        = Fn
module Force_once                = Force_once
module Fqueue                    = Fqueue
module Gc                        = Core_gc
module Hash_heap                 = Hash_heap
module Hash_queue                = Hash_queue
module Hash_set                  = Hash_set
module Hashable                  = Hashable
module Hashtbl_intf              = Core_hashtbl_intf
module Heap                      = Heap
module Heap_block                = Heap_block
module Host_and_port             = Host_and_port
module Identifiable              = Identifiable
module In_channel                = In_channel
module Info                      = Info
module Int63                     = Core_int63
module Int_conversions           = Int_conversions
module Int_intf                  = Int_intf
module Int_set                   = Int_set
module Interfaces                = Interfaces
module Invariant                 = Invariant
module Linked_stack              = Linked_stack
module Maybe_bound               = Maybe_bound
module Memo                      = Memo
module Monad                     = Monad
module Month                     = Month
module No_polymorphic_compare    = No_polymorphic_compare
module Nothing                   = Nothing
module Only_in_test              = Only_in_test
module Option                    = Option
module Ordered_collection_common = Ordered_collection_common
module Or_error                  = Or_error
module Out_channel               = Out_channel
module Percent                   = Percent
module Pid                       = Pid
module Poly                      = Poly
module Polymorphic_compare       = Polymorphic_compare
module Pool                      = Pool
module Pooled_hashtbl            = Pooled_hashtbl
module Pretty_printer            = Pretty_printer
module Printexc                  = Core_printexc
module Printf                    = Core_printf
module Quickcheck_intf           = Quickcheck_intf
module Quickcheck                = Quickcheck
module Quickcheckable            = Quickcheckable
module Result                    = Result
module Robustly_comparable       = Robustly_comparable
module Rope                      = Rope
module Sequence                  = Sequence
module Set_once                  = Set_once
module Sexp_maybe                = Core_sexp.Sexp_maybe
module Sexpable                  = Sexpable
module Sign                      = Sign
module Source_code_position      = Source_code_position
module Stack                     = Core_stack
module Staged                    = Staged
module String_id                 = String_id
module Stringable                = Stringable
module Substring                 = Substring
module Substring_intf            = Substring_intf
module Thread_safe_queue         = Thread_safe_queue
module Timing_wheel_ns           = Timing_wheel_ns
module Total_map                 = Total_map
module Tuple                     = Tuple
module Tuple2                    = Tuple.T2
module Tuple3                    = Tuple.T3
module Type_equal                = Type_equal
module Type_immediacy            = Type_immediacy
module Union_find                = Union_find
module Unique_id                 = Unique_id
module Unit                      = Unit
module Univ                      = Univ
module Univ_map                  = Univ_map
module Unpack_buffer             = Unpack_buffer
module Validate                  = Validate
module Validated                 = Validated
module Weak                      = Core_weak
module With_return               = With_return
module Word_size                 = Word_size

module type Unique_id = Unique_id.Id
module type Validated = Validated.Validated

include T

type 'a _maybe_bound = 'a Maybe_bound.t =
    Incl of 'a | Excl of 'a | Unbounded

type decimal = Decimal.t [@@deriving bin_io, sexp, compare]

(* Some people have proposed removing [does_raise], but there isn't consensus. *)
let does_raise = Exn.does_raise

type bytes =
  [ `This_type_does_not_equal_string_because_we_want_type_errors_to_say_string ]
;;

let () =
  Backtrace.initialize_module ();
  Exn.initialize_module ();
;;
