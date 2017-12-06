(** A functor for displaying a type as a sequence of ASCII characters printed in
    hexadecimal.

    [sexp_of_t] and [to_string_hum] print [t] in a similar format to 'hexdump' on Unix
    systems.  For example, the string "Back off, man, I'm a scientist." renders as:

    {v
00000000  42 61 63 6b 20 6f 66 66  2c 20 6d 61 6e 2c 20 49  |Back off, man, I|
00000010  27 6d 20 61 20 73 63 69  65 6e 74 69 73 74 2e     |'m a scientist.|
0000001f v}

    [to_sequence] produces a sequence of strings representing lines in the hex dump.  It
    can be used to process a hex dump incrementally, for example with potentially infinite
    values, or to avoid keeping the entire output in memory at once. *)

include Hexdump_intf.Hexdump (** @inline *)
