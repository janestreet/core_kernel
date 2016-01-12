(** A rope is a standard data structure that represents a single string as a tree of
    strings, and supports constant-time concatenation.  The [Rope] module provides a
    subset of the interface of a standard rope implementation.  A more complete
    implementation (including Unicode support) is available in the zed library.

    Rope concatenation is unlike n-ary string concatenation, [s1 ^ s2 ^ s3 ...], which is
    quadratic in the number of strings, with each [^] allocating a string.  Rope
    concatenation:

    {[
      Rope.(to_string (of_string s1 ^ of_string s2 ^ of_string s3 ^ ...))
    ]}

    is linear, because each [Rope.of_string] and [Rope.(^)] is constant time, and
    [Rope.to_string] allocates a single string and then copies all the inputs into it.

    Similarly, [String.concat [ s1; s2; s3; ... ]] allocates a single string and copies
    the inputs into it -- so [Rope] is no improvement over that usage.  [Rope] becomes
    useful when the construction of the sequence of strings is more complex, e.g.
    appending on both sides, or recursion.
*)

type t

(** takes O(1) time *)
val of_string : string -> t

(** takes time proportional to [n+m] where [n] is the total size of the result and [m]
    is the number of strings being concatenated *)
val to_string : t -> string

(** takes O(1) time *)
val ( ^ ) : t -> t -> t

(** Appends the contents of the Rope at the end of a destination buffer *)
val add_to_buffer : t -> Buffer.t -> unit
