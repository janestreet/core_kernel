(** 63 or 64 bit integers.

    The size of Int63 is always at least 63 bits.  On a 64-bit platform it is just an int
    (63-bits), and on a 32-bit platform it is an int64.

    Because Int63 has different sizes on 32-bit and 64-bit platforms, there are several
    pitfalls to be aware of:

    - Int63 will behave differently in the case of overflow.

    - marshalling Int63 will not work between 32-bit and 64-bit platforms.
    unmarshal will segfault.

    - bin_io will work, except that it will raise an overflow exception when you send too
      large of an int from a 32-bit to a 64-bit platform.  This is couterintuitive because
      the 32-bit platform has the larger int size. *)

INCLUDE "config.mlh"

IFDEF ARCH_SIXTYFOUR THEN

(** We expose [private int] so that the compiler can omit caml_modify when dealing with
    record fields holding [Int63.t].  Code should not explicitly make use of the
    [private], e.g. via [(i :> int)], since such code will not compile on 32-bit
    platforms. *)
include Int_intf.S with type t = private int

ELSE

include Int_intf.S

ENDIF

val of_int : int -> t
val to_int : t -> int option

