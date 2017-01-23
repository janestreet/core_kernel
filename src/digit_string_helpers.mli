(** Utility functions for parsing and outputing strings containing known numbers
    of digits.  Used primarily for building functions for reading in and writing
    out Time related values. *)

open! Import

val char_of_digit : int -> Char.t

(** [blit_string_of_int_4_digits s ~pos i] blits the string representation of [i] into
    [s] at [pos].  Raises unless 0 <= [i] <= 9999 and there is sufficient room. *)
val blit_string_of_int_4_digits : string -> pos:int -> int -> unit

(** [blit_string_of_int_3_digits s ~pos i] blits the string representation of [i] into
    [s] at [pos].  Raises unless 0 <= [i] <= 999 and there is sufficient room. *)
val blit_string_of_int_3_digits : string -> pos:int -> int -> unit

(** [blit_string_of_int_2_digits s ~pos i] blits the string representation of [i] into
    [s] at [pos].  Raises unless 0 <= [i] <= 99 and there is sufficient room. *)
val blit_string_of_int_2_digits : string -> pos:int -> int -> unit

(** [parse_two_digits s pos] parse two digits at [pos] in [s] into the corresponding
    int. *)
val parse_two_digits : string -> int -> int

(** [parse_four_digits s pos] parse four digits at [pos] in [s] into the corresponding
    int. *)
val parse_four_digits : string -> int -> int
