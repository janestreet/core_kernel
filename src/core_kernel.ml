open! Import

module Std = struct
  include Std_kernel
  include Std_common

  module Container_intf                  = Container_intf
  module Core_hashtbl_intf               = Core_hashtbl_intf
  module Core_map_intf                   = Core_map_intf
  module Core_set_intf                   = Core_set_intf
  module Int_replace_polymorphic_compare = Int_replace_polymorphic_compare
  module Obj_array                       = Obj_array
  module Polymorphic_compare_intf        = Polymorphic_compare_intf
  module Stack_intf                      = Stack_intf
  module Timing_wheel_ns_intf            = Timing_wheel_ns_intf
end

include Std
module Stable = Stable

module Bigbuffer_internal              = Bigbuffer_internal
module Container_unit_tests            = Container_unit_tests
module Ofday                           = Ofday
module Perms                           = Perms
module Span                            = Span
module Stable_containers               = Stable_containers
module Stable_internal                 = Stable_internal
module Std_internal                    = Std_internal
module Std_kernel                      = Std_kernel
module Time_internal                   = Time_internal
module Time_ns_alternate_sexp          = Time_ns_alternate_sexp
