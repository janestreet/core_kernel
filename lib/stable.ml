module Unit_test = Stable_unit_test.Make

(* miscellaneous unit tests that can't be put in their respective .mls due to circular
   dependencies *)
module Unit_tests = struct
  TEST_MODULE "Result.V1" = Unit_test(Result.Stable.V1_stable_unit_test)
end

include Stable_internal

include Stable_containers
module Blang          = Blang         .Stable
module Host_and_port  = Host_and_port .Stable
module Month          = Month         .Stable
module Nothing        = Nothing       .Stable
module Percent        = Percent       .Stable
module Result         = Result        .Stable
module String_id      = String_id     .Stable
module Day_of_week    = Day_of_week   .Stable


