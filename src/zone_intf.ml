(** Time-zone handling. *)

open! Import

module type S = sig
  module Time : Time0_intf.S

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

  (** [abbreviation t time] returns the abbreviation name (such as EDT, EST, JST) of given
      zone [t] at [time].  This string conversion is one-way only, and cannot reliably be
      turned back into a [t]. *)
  val abbreviation : t -> Time.t -> string

  val name : t -> string

  (** [original_filename t] return the filename [t] was loaded from (if any) *)
  val original_filename : t -> string option

  (** [digest t] return the MD5 digest of the file the t was created from (if any) *)
  val digest : t -> string option

  (** [shift_epoch_time zone [`Local | `UTC] time] Takes an epoch (aka "unix") time given
      either in local or in UTC (as indicated in the arguments) and shifts it according to
      the local time regime in force in zone.  That is, given a Local epoch time it will
      return the corresponding UTC timestamp and vice versa.  This function is low level,
      and is not intended to be called by most client code.  Use the high level functions
      provided in Time instead. *)
  val shift_epoch_time : t -> [`Local | `UTC] -> Time.t -> Time.t

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
end

module type Zone = sig
  module type S = S

  module Make (Time : Time0_intf.S) : S with module Time := Time
end
