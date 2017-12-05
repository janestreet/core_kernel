(** Time-zone handling. *)

open! Import

module type S_common = sig
  (** {1 User-friendly interface} *)

  (** The type of a time-zone.

      bin_io and sexp representations of Zone.t are the name of the zone, and
      not the full data that is read from disk when Zone.find is called.  The
      full Zone.t is reconstructed on the receiving/reading side by reloading
      the zone file from disk.  Any zone name that is accepted by [find] is
      acceptable in the bin_io and sexp representations. *)
  type t [@@deriving sexp_of, compare]

  (** [input_tz_file ~zonename ~filename] read in [filename] and return [t]
      with [name t] = [zonename] *)
  val input_tz_file : zonename:string -> filename:string -> t

  (** [likely_machine_zones] is a list of zone names that will be searched
      first when trying to determine the machine zone of a box.  Setting this
      to a likely set of zones for your application will speed the very first
      use of the local timezone. *)
  val likely_machine_zones : string list ref

  (** [of_utc_offset offset] returns a timezone with a static UTC offset (given in
      hours). *)
  val of_utc_offset : hours:int -> t

  (** [utc] the UTC time zone.  Included for convenience *)
  val utc : t

  val name : t -> string

  (** [original_filename t] return the filename [t] was loaded from (if any) *)
  val original_filename : t -> string option

  (** [digest t] return the MD5 digest of the file the t was created from (if any) *)
  val digest : t -> Md5.t option

  module Full_data : sig
    module Stable : sig
      module V1 : Stable_module_types.S0_without_comparator with type t = t
    end
  end
end

module type S = sig
  module Time : Time0_intf.S

  include S_common

  (** [abbreviation t time] returns the abbreviation name (such as EDT, EST, JST) of given
      zone [t] at [time].  This string conversion is one-way only, and cannot reliably be
      turned back into a [t]. *)
  val abbreviation : t -> Time.t -> string

  (** [absolute_time_of_relative_time] and [relative_time_of_absolute_time] convert times
      between absolute (time from epoch in UTC) and relative (shifted according to time
      zone and daylight savings) forms. These are low level functions not intended for
      most clients. *)
  val absolute_time_of_relative_time : t -> Time.Relative_to_unspecified_zone.t -> Time.t
  val relative_time_of_absolute_time : t -> Time.t -> Time.Relative_to_unspecified_zone.t

  (** Takes a [Time.t] and returns the next [Time.t] strictly after it, if any, that the
      time zone UTC offset changes, and by how much it does so. *)
  val next_clock_shift
    :  t
    -> after:Time.t
    -> (Time.t * Time.Span.t) option

  (** As [next_clock_shift], but strictly *before* the given time. *)
  val prev_clock_shift
    :  t
    -> before:Time.t
    -> (Time.t * Time.Span.t) option

  (** For performance testing only; [reset_transition_cache t] resets an internal cache in
      [t] used to speed up repeated lookups of the same clock shift transition. *)
  val reset_transition_cache : t -> unit
end

module type Zone = sig
  module type S = S

  include S_common

  module Make (Time : Time0_intf.S) : S with type t = t and module Time := Time
end
