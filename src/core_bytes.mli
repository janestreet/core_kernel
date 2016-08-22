(** OCaml's built in [bytes] type, currently equal to [string]. *)

type t = bytes [@@deriving bin_io, compare, sexp]

include Container.S0   with type t := t with type elt := char
include Blit.S         with type t := t
include Identifiable.S with type t := t
module To_string   : Blit.S_distinct with type src := t with type dst := string
module From_string : Blit.S_distinct with type src := string with type dst := t

(** [length t] returns the length (number of bytes) of [t]. *)
external length: t -> int = "%string_length"

(** [get s n] returns the byte at index [n] in [s].
    Raise [Invalid_argument] if [n] not a valid index in [s]. *)
external get : t -> int -> char = "%string_safe_get"

(** [set s n c] modifies [s] in place, replacing the byte at index [n] 
    with [c]. Raise [Invalid_argument] if [n] is not a valid index in [s]. *)
external set : t -> int -> char -> unit = "%string_safe_set"

(** [create n] returns a new byte sequence of length [n]. The
    sequence is uninitialized and contains arbitrary bytes.
    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)
external create : int -> t = "caml_create_string"

(** [make n c] returns a new byte sequence of length [n], filled with
    the byte [c]. 
    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)
val make : int -> char -> t

(** [init n ~f] returns a fresh byte sequence of length [n], with character [i] 
    initialized to the result of [f i] (in increasing index order).
    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)
val init : int -> f:(int -> char) -> t

(** [empty] a byte sequence of size 0. *)
val empty : t
  
(** [copy t] returns a new byte sequence that contains the same bytes as [t]. *)
val copy : t -> t

(** [of_string s] returns a new byte sequence that contains the
    same bytes as the given string. *)
val of_string : string -> t

(** [to_string t] returns a new string that contains the same   bytes as the 
    given byte sequence. *)
val to_string : t -> string

(** [extend s left right] returns a new byte sequence that contains
    the bytes of [s], with [left] uninitialized bytes prepended and
    [right] uninitialized bytes appended to it. If [left] or [right]
    is negative, then bytes are removed (instead of appended) from
    the corresponding side of [s].
    Raise [Invalid_argument] if the result length is negative or
    longer than {!Sys.max_string_length} bytes. *)
val extend : t -> int -> int -> t

(** [fill s start len c] modifies [s] in place, replacing [len]
    characters with [c], starting at [start].
    Raise [Invalid_argument] if [start] and [len] do not designate a
    valid range of [s]. *)
val fill : t -> int -> int -> char -> unit

(** [concat sep sl] concatenates the list of byte sequences [sl],
    inserting the separator byte sequence [sep] between each, and
    returns the result as a new byte sequence.
    Raise [Invalid_argument] if the result is longer than
   {!Sys.max_string_length} bytes. *)
val concat : t -> t list -> t

(** [cat s1 s2] concatenates [s1] and [s2] and returns the result
    as new byte sequence.
    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes. *)
val cat : t -> t -> t

(** [iteri t ~f] same as {!iter}, but the function is 
    applied to the index of the byte as first argument and the 
    byte itself as second argument. *)
val iteri : t -> f:(int -> char -> unit) -> unit

(** [map s ~f] applies function [f] in turn to all the bytes of [s]
    (in increasing index order) and stores the resulting bytes in
    a new sequence that is returned as the result. *)
val map : t -> f:(char -> char) -> t

(** [mapi s ~f] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the resulting bytes
    in a new sequence that is returned as the result. *)
val mapi : t -> f:(int -> char -> char) -> t

(** [trim t] returns a copy of [t], without leading and trailing
    whitespace. The bytes regarded as whitespace are the ASCII
    characters [' '], ['\012'], ['\n'], ['\r'], and ['\t']. *)
val trim : t -> t

(** [escaped t] returns a copy of [t], with special characters
    represented by escape sequences, following the lexical 
    conventions of OCaml.
    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes. *)
val escaped : t -> t

(** [index s c] returns the index of the first occurrence of byte [c]
    in [s]. Raise [Not_found] if [c] does not occur in [s]. *)
val index : t -> char -> int

(** [rindex s c] returns the index of the last occurrence of byte [c]
    in [s]. Raise [Not_found] if [c] does not occur in [s]. *)
val rindex : t -> char -> int

(** [index_from s i c] returns the index of the first occurrence of
    byte [c] in [s] after position [i].  [index s c] is
    equivalent to [index_from s 0 c].
    Raise [Invalid_argument] if [i] is not a valid position in [s].
    Raise [Not_found] if [c] does not occur in [s] after position [i]. *)
val index_from : t -> int -> char -> int

(** [rindex_from s i c] returns the index of the last occurrence of
    byte [c] in [s] before position [i+1].  [rindex s c] is equivalent
    to [rindex_from s (length s - 1) c].
    Raise [Invalid_argument] if [i+1] is not a valid position in [s].
    Raise [Not_found] if [c] does not occur in [s] before position [i+1]. *)
val rindex_from : t -> int -> char -> int

(** [contains s c] tests if byte [c] appears in [s]. *)
val contains : t -> char -> bool

(** [contains_from s start c] tests if byte [c] appears in [s] after
    position [start].  [contains s c] is equivalent to [contains_from
    s 0 c]. Raise [Invalid_argument] if [start] is not a valid position in [s]. *)
val contains_from : t -> int -> char -> bool

(** [rcontains_from s stop c] tests if byte [c] appears in [s] before
    position [stop+1].
    Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
    position in [s]. *)
val rcontains_from : t -> int -> char -> bool

(** [uppercase t] returns a copy of [t], with all lowercase letters
    translated to uppercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)
val uppercase : t -> t

(** [lowercase t] returns a copy of [t], with all uppercase letters
    translated to lowercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)
val lowercase : t -> t

(** [capitalize t] returns a copy of [t], with the first byte set 
    to uppercase. *)
val capitalize : t -> t

(** [uncapitalize t] returns a copy of [t], with the first byte set 
    to lowercase. *)
val uncapitalize : t -> t

(** {4 Unsafe conversions (for advanced users)}

    This section describes unsafe, low-level conversion functions
    between [bytes] and [string]. They do not copy the internal data;
    used improperly, they can break the immutability invariant on
    strings provided by the [-safe-string] option. They are available for
    expert library authors, but for most purposes you should use the
    always-correct {!Bytes.to_string} and {!Bytes.of_string} instead. *)
module Unsafe : sig

(** [to_string b] - unsafely converts a byte sequence into a string.

    To reason about the use of [to_string], it is convenient to
    consider an "ownership" discipline. A piece of code that
    manipulates some data "owns" it; there are several disjoint ownership
    modes, including:
    - Unique ownership: the data may be accessed and mutated
    - Shared ownership: the data has several owners, that may only
      access it, not mutate it.

    Unique ownership is linear: passing the data to another piece of
    code means giving up ownership (we cannot write the
    data again). A unique owner may decide to make the data shared
    (giving up mutation rights on it), but shared data may not become
    uniquely-owned again.

    [to_string s] can only be used when the caller owns the byte
    sequence [s] -- either uniquely or as shared immutable data. The
    caller gives up ownership of [s], and gains ownership of the
    returned string.

    There are two valid use-cases that respect this ownership
    discipline:

    1. Creating a string by initializing and mutating a byte sequence
    that is never changed after initialization is performed.
    {[
      let string_init len f : string =
      let s = Bytes.create len in
      for i = 0 to len - 1 do Bytes.set s i (f i) done;
      Bytes.Unsafe.to_string s
    ]}

    This function is safe because the byte sequence [s] will never be
    accessed or mutated after [to_string] is called. The
    [string_init] code gives up ownership of [s], and returns the
    ownership of the resulting string to its caller.
    Note that it would be unsafe if [s] was passed as an additional
    parameter to the function [f] as it could escape this way and be
    mutated in the future -- [string_init] would give up ownership of
    [s] to pass it to [f], and could not call [to_string]
    safely.

    We have provided the {!String.init}, {!String.map} and
    {!String.mapi} functions to cover most cases of building
    new strings. You should prefer those over [to_string] or
    [to_string] whenever applicable.

    2. Temporarily giving ownership of a byte sequence to a function
    that expects a uniquely owned string and returns ownership back, so
    that we can mutate the sequence again after the call ended.

    {[
      let bytes_length (s : bytes) =
        String.length (Bytes.Unsafe.to_string s)
    ]}

    In this use-case, we do not promise that [s] will never be mutated
    after the call to [bytes_length s]. The {!String.length} function
    temporarily borrows unique ownership of the byte sequence
    (and sees it as a [string]), but returns this ownership back to
    the caller, which may assume that [s] is still a valid byte
    sequence after the call. Note that this is only correct because we
    know that {!String.length} does not capture its argument -- it could
    escape by a side-channel such as a memoization combinator.

    The caller may not mutate [s] while the string is borrowed (it has
    temporarily given up ownership). This affects concurrent programs,
    but also higher-order functions: if [String.length] returned
    a closure to be called later, [s] should not be mutated until this
    closure is fully applied and returns ownership. *)
  val to_string : t -> string

(** [of_string s] - unsafely converts a shared string to a byte 
    sequence that should not be mutated.

    The same ownership discipline that makes [to_string]
    correct applies to [of_string]: you may use it if you were
    the owner of the [string] value, and you will own the return
    [bytes] in the same mode.
 
    In practice, unique ownership of string values is extremely
    difficult to reason about correctly. You should always assume
    strings are shared, never uniquely owned.

    For example, string literals are implicitly shared by the
    compiler, so you never uniquely own them.

    {[
      let incorrect = Bytes.Unsafe.of_string "hello"
      let s = Bytes.of_string "hello"
    ]}

    The first declaration is incorrect, because the string literal
    ["hello"] could be shared by the compiler with other parts of the
    program, and mutating [incorrect] is a bug. You must always use
    the second version, which performs a copy and is thus correct.

    Assuming unique ownership of strings that are not string
    literals, but are (partly) built from string literals, is also
    incorrect. For example, mutating [of_string ("foo" ^ s)]
    could mutate the shared string ["foo"] -- assuming a rope-like
    representation of strings. More generally, functions operating on
    strings will assume shared ownership, they do not preserve unique
    ownership. It is thus incorrect to assume unique ownership of the
    result of [of_string].

    The only case we have reasonable confidence is safe is if the
    produced [bytes] is shared -- used as an immutable byte
    sequence. This is possibly useful for incremental migration of
    low-level programs that manipulate immutable sequences of bytes
    (for example {!Marshal.from_bytes}) and previously used the
    [string] type for this purpose. *)
  val of_string : string -> t

  (** The following is for system use only. Do not call directly. *)
  external get  : t -> int -> char = "%string_unsafe_get"
  external set  : t -> int -> char -> unit = "%string_unsafe_set"
  external blit : t -> int -> t -> int -> int -> unit = "caml_blit_string" "noalloc"
  external fill : t -> int -> int -> char -> unit = "caml_fill_string" "noalloc"

end
