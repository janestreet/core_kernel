open! Import


(**/**)
module Std = struct
  include Std_kernel (** @inline *)

  module Bigbuffer                       = Bigbuffer
  module Bigstring                       = Bigstring
  module Bigstring_marshal               = Bigstring_marshal
  module Caml                            = Caml
  module Container_intf                  = Container_intf
  module Core_kernel_stable              = Stable
  module Date                            = Date
  module Int_replace_polymorphic_compare = Int_replace_polymorphic_compare
  module Map_intf                        = Map_intf
  module Md5                             = Md5
  module Digest                          = Md5
  [@@ocaml.deprecated "[since 2017-05] Use Md5 instead."]
  module Optional_syntax_intf            = Optional_syntax_intf
  module Perms                           = Perms
  module Set_intf                        = Set_intf
  module Stack_intf                      = Stack_intf
  module Time                            = Time_float
  module Time_ns                         = Time_ns
  module Timing_wheel_ns_intf            = Timing_wheel_ns_intf
  module Version_util                    = Version_util

  (** To be used in implementing Core, but not by end users *)
  module Core_kernel_private = struct
    module Digit_string_helpers = Digit_string_helpers
    module Time0_intf           = Time0_intf
    module Time_intf            = Time_intf
    module Time_float0          = Time_float0
    module Kernel_time          = Time
    module Time_float           = Time_float
    module Ofday_parser         = Ofday_parser

    module Bigbuffer_internal              = Bigbuffer_internal
    module Container_unit_tests            = Container_unit_tests
    module Stable_internal                 = Stable_internal
    module Std_internal                    = Std_internal

    module Std_kernel                      = Std_kernel
    (** [Std_kernel] defines modules exposed by [Core_kernel] that are not overridden by
        [Core].  It is used in [core.ml] to re-export these modules. *)

    module Time_ns_alternate_sexp          = Time_ns_alternate_sexp
  end
end
(**/**)

include Std (** @inline *)
