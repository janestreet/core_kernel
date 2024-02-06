(** Manipulating version util and build info "sections" of data stored in executables. *)

open! Core

(** Create a section. Each section has a fixed length and starts with a marker string. *)
module Make : functor
  (M : sig
     val name : string
     val start_marker : string
     val length_including_start_marker : int
   end)
  -> sig
  (** Extract the first section from an executable, including the start marker. Returns
        [None] if there are no matching sections. *)
  val get : contents_of_exe:string -> string option

  (** Find all occurrences of the section and replace their payloads with new [data].
        Returns [None] if there are no occurrences. *)
  val replace : contents_of_exe:string -> data:string -> string option

  (** An alias for [String.chop_prefix_if_exists ~prefix:M.start_marker]. *)
  val chop_start_marker_if_exists : string -> string

  (** How many times does the marker appear in the executable? *)
  val count_occurrences : contents_of_exe:string -> int

  module Expert : sig
    (** The start marker, as passed via the functor's argument. It's in the [Expert]
          module because one is not supposed to fiddle with start markers directly. *)
    val start_marker : string

    (** Pad a string with NUL bytes up to the length of the section's payload.

          Raises if there is no space for a NUL byte or if the given string contains a NUL
          byte already. *)
    val pad_with_at_least_one_nul_byte_exn : string -> string
  end
end
