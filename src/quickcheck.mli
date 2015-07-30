(** You should start reading in quickcheck_generator.mli *)

open Quickcheck_intf

include Quickcheck  (** with a default config *)

module Configure (Config : Quickcheck_config) : Quickcheck
