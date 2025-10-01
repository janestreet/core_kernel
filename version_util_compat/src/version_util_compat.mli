(** Implements [Command__Command_intf.Version_util]. This is checked in [../test/] but not
    asserted here to avoid a dependency on [Command]. *)

open Core

module Time : sig
  type t [@@deriving sexp_of]
end

val version_list : string list
val reprint_build_info : (Time.t -> Sexp.t) -> string

(** This value is used by [Inline_benchmarks_kernel_private], part of [Core_bench], which
    is a public library. *)
val x_library_inlining : bool
