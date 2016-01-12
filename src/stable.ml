module Unit_test = Stable_unit_test.Make

(* miscellaneous unit tests that can't be put in their respective .mls due to circular
   dependencies *)
module Unit_tests = struct
  let%test_module "Result.V1" = (module Unit_test(Result.Stable.V1_stable_unit_test))
end

module type Stable  = Stable_module_types.S0
module type Stable1 = Stable_module_types.S1
module type Stable2 = Stable_module_types.S2

include Stable_internal

include Stable_containers
module Binable       = Binable       .Stable
module Blang         = Blang         .Stable
module Day_of_week   = Day_of_week   .Stable
module Either        = Either        .Stable
module Error         = Error         .Stable
module Fdeque        = Fdeque        .Stable
module Fqueue        = Fqueue        .Stable
module Host_and_port = Host_and_port .Stable
module Info          = Info          .Stable
module Lazy          = Core_lazy     .Stable
module Month         = Month         .Stable
module Nothing       = Nothing       .Stable
module Or_error      = Or_error      .Stable
module Percent       = Percent       .Stable
module Perms         = Perms         .Stable
module Result        = Result        .Stable
module Sexpable      = Sexpable      .Stable
module String_id     = String_id     .Stable

include Perms.Export
