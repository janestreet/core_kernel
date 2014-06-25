(** An extension of the standard StringLabels. If you open Core.Std, you'll get
    these in the String module. *)

type t = string with bin_io, sexp, typerep

include Blit.S         with type t := t
include Container.S0   with type t := t with type elt = char
include Identifiable.S with type t := t

(** Maximum length of a string. *)
val max_length : int

(* From the standard StringLabels *)
external length : t -> int = "%string_length"

external get : t -> int -> char = "%string_safe_get"
external set : t -> int -> char -> unit = "%string_safe_set"

external create : int -> t = "caml_create_string"
val make : int -> char -> t
val copy : t -> t
val init : int -> f:(int -> char) -> t

val fill : t -> pos:int -> len:int -> char -> unit

(** concatanate all strings in the list using separator [sep] (default sep "") *)
val concat : ?sep:t -> t list -> t

(* (** Like concat, but uses the Container typeclass *)
val tc_concat : (t, 'container) Container.tc -> sep:t -> 'container -> t *)

(** Warning: Only returns a copy if changes are necessary!  Special characters are
    represented by escape sequences, following the lexical conventions of Objective
    Caml. *)
val escaped : t -> t

val contains : ?pos:int -> ?len:int -> t -> char -> bool

val uppercase : t -> t
val lowercase : t -> t

val capitalize   : t -> t
val uncapitalize : t -> t

val index      : t -> char -> int option
val index_exn  : t -> char -> int
val rindex     : t -> char -> int option
val rindex_exn : t -> char -> int

val index_from      : t -> int -> char -> int option
val index_from_exn  : t -> int -> char -> int
val rindex_from     : t -> int -> char -> int option
val rindex_from_exn : t -> int -> char -> int

(** Substring search and replace functions.  They use the Knuth-Morris-Pratt algorithm
    (KMP) under the hood.

    The functions in the [Search_pattern] module allow the program to preprocess the
    searched pattern once and then use it many times without further allocations. *)
module Search_pattern : sig

  type t with sexp_of

  (** [create pattern] preprocesses [pattern] as per KMP, building an [int array] of
      length [length pattern].  All inputs are valid. *)
  val create : string -> t

  (** [pos < 0] or [pos >= length string] result in no match (hence [index] returns
      [None] and [index_exn] raises). *)
  val index     : ?pos:int -> t -> in_:string -> int option
  val index_exn : ?pos:int -> t -> in_:string -> int

  (** [may_overlap] determines whether after a successful match, [index_all] should start
      looking for another one at the very next position ([~may_overlap:true]), or jump to
      the end of that match and continue from there ([~may_overlap:false]), e.g.:

      {|
        index_all (create "aaa") ~may_overlap:false ~in_:"aaaaBaaaaaa" = [0; 5; 8]
        index_all (create "aaa") ~may_overlap:true  ~in_:"aaaaBaaaaaa" = [0; 1; 5; 6; 7; 8]
      |}

      E.g. [replace_all] internally calls [index_all ~may_overlap:false].
  *)
  val index_all : t -> may_overlap:bool -> in_:string -> int list

  (** Note that the result of [replace_all pattern ~in_:text ~with_:r] may still
      contain [pattern], e.g.

      {[
        replace_all (create "bc") ~in:"aabbcc" ~with_:"cb" = "aabcbc"
      ]}
  *)
  val replace_first : ?pos:int -> t -> in_:string -> with_:string -> string
  val replace_all   :             t -> in_:string -> with_:string -> string
end

(** Substring search and replace convenience functions.  They call [Search_pattern.create] and
    then forget the preprocessed pattern when the search is complete.  [pos < 0] or [pos
    >= length t] result in no match (hence [substr_index] returns [None] and
    [substr_index_exn] raises).  [may_overlap] indicates whether to report overlapping
    matches, see [Search_pattern.index_all]. *)
val substr_index     : ?pos:int -> t -> pattern:t -> int option
val substr_index_exn : ?pos:int -> t -> pattern:t -> int
val substr_index_all : t -> may_overlap:bool -> pattern:t -> int list

val substr_replace_first : ?pos:int -> t -> pattern:t -> with_:t -> t
(** As with [Search_pattern.replace_all], the result may still contain [pattern]. *)
val substr_replace_all   :             t -> pattern:t -> with_:t -> t

(** [slice s start stop] gets a slice of [s] between [start] and [stop].
    [start] and [stop] will be normalized before the access.
    (viz. Core_array.normalize). *)
val slice : t -> int -> int -> t

(** Returns the reversed list of characters contained in a list. *)
val to_list_rev : t -> char list

(** [rev t] returns [t] in reverse order. *)
val rev : t -> t

(** [nget s i] Gets the char at normalized position [i] in [s]. *)
val nget : t -> int -> char

(** [nset s i c] Sets the char at normalized position [i] to [c]. *)
val nset : t -> int -> char -> unit

(** [is_suffix s ~suffix] returns [true] if [s] ends with [suffix]. *)
val is_suffix : t -> suffix:t -> bool

(** [is_prefix s ~prefix] returns [true] if [s] starts with [prefix]. *)
val is_prefix : t -> prefix:t -> bool

(** If the string [s] contains the character [on], then [lsplit2_exn
    s ~on] returns a pair containing [s] split around the first
    appearance of [on] (from the left).
    @raise Not_found When [on] cannot be found in [s]
*)
val lsplit2_exn : t -> on:char -> t * t

(** If the string [s] contains the character [on], then [rsplit2_exn
    s ~on] returns a pair containing [s] split around the first
    appearance of [on] (from the right).
    @raise Not_found When [on] cannot be found in [s]
*)
val rsplit2_exn : t -> on:char -> t * t

(** [lsplit2 line ~on] optionally returns [line] split into two strings around the
  * first appearance of [on] from the left *)
val lsplit2 : t -> on:char -> (t * t) option

(** [rsplit2 line ~on] optionally returns [line] split into two strings around the
  * first appearance of [on] from the right *)
val rsplit2 : t -> on:char -> (t * t) option

(** [split s ~on] @return a list of substrings of [s] that are separated by
    [on].  Consecutive [on] characters will cause multiple empty strings
    in the result.  Splitting the empty string returns a list of the empty
    string, not the empty list. *)
val split : t -> on:char -> t list

(** [split_on_chars s ~on] @return a list of all substrings of [s]
    that are separated by one of the chars from [on].  [on]
    are not grouped.  So a grouping of [on] in the source string will
    produce multiple empty string splits in the result.  *)
val split_on_chars : t -> on:char list -> t list

(** [split_lines t] returns the list of lines that comprise [t].  The lines do
    not include the trailing ["\n"] or ["\r\n"]. *)
val split_lines : t -> t list

(** [lfindi ?pos t ~f] returns the smallest [i >= pos] such that [f i t.[i]], if there is
    such an [i].  By default, [pos = 0]. *)
val lfindi : ?pos : int -> t -> f:(int -> char -> bool) -> int option

(** [rfindi ?pos t ~f] returns the largest [i <= pos] such that [f i t.[i]], if there is
    such an [i].  By default [pos = length t - 1]. *)
val rfindi : ?pos : int -> t -> f:(int -> char -> bool) -> int option

(* Warning: the following strip functions have copy-on-write semantics (i.e. they may
   return the same string passed in) *)

(** [lstrip ?drop s] returns a string with consecutive chars satisfying [drop] (by default
    white space, e.g. tabs, spaces, newlines, and carriage returns) stripped from the
    beginning of [s]. *)
val lstrip : ?drop:(char -> bool) -> t -> t

(** [rstrip ?drop s] returns a string with consecutive chars satisfying [drop] (by default
    white space, e.g. tabs, spaces, newlines, and carriage returns) stripped from the end
    of [s]. *)
val rstrip : ?drop:(char -> bool) -> t -> t

(** [strip ?drop s] returns a string with consecutive chars satisfying [drop] (by default
    white space, e.g. tabs, spaces, newlines, and carriage returns) stripped from the
    beginning and end of [s]. *)
val strip : ?drop:(char -> bool) -> t -> t

(** [map f s] applies [f] to each character in [s], and returns the
    resulting string. *)
val map : t -> f : (char -> char) -> t

(** [mapi f s] applies [f] to each character in [s] and its index, and returns the
    resulting string. *)
val mapi : t -> f : (int -> char -> char) -> t

(** [foldi] works similarly to [fold], but also pass in index of each character to [f] *)
val foldi : t -> init : 'a -> f : (int -> 'a -> char -> 'a) -> 'a

(** Like [map], but allows replacement of a single character with zero or two or more
    characters. *)
val concat_map : ?sep:t -> t -> f : (char -> t) -> t

(** [filter s ~f:predicate] discards characters not satisfying [predicate] *)
val filter : t -> f : (char -> bool) -> t

(** [tr target replacement s] replaces every instance of [target] in [s] with
    [replacement]. *)
val tr : target : char -> replacement : char -> t -> t

(** [tr_inplace target replacement s] destructively modifies s (in place!)
    replacing every instance of [target] in [s] with [replacement]. *)
val tr_inplace : target : char -> replacement : char -> t -> unit

(** [chop_suffix s ~suf] returns a copy [s] without the trailing [suff]
    @raise Invalid_argument is [suff] is not a suffix of [s]
*)
val chop_suffix_exn : t -> suffix:t -> t

(** [chop_prefix s ~pref] returns a copy [s] without the leading [pref]
    @raise Invalid_argument is [pref] is not a prefix of [s]
*)
val chop_prefix_exn : t -> prefix:t -> t

val chop_suffix : t -> suffix:t -> t option

val chop_prefix : t -> prefix:t -> t option

(** [suffix s n] returns the longest suffix of [s] of length less than or equal to [n] *)
val suffix : t -> int -> t

(** [prefix s n] returns the longest prefix of [s] of length less than or equal to [n] *)
val prefix : t -> int -> t

(** [drop_suffix s n] drops the longest suffix of [s] of length less than or equal to [n] *)
val drop_suffix : t -> int -> t

(** [drop_prefix s n] drops the longest prefix of [s] of length less than or equal to [n] *)
val drop_prefix : t -> int -> t

(** [concat_array sep ar] like {!String.concat}, but operates on arrays *)
val concat_array : ?sep : t -> t array -> t

(** slightly faster hash function on strings *)
external hash : t -> int = "caml_hash_string" "noalloc"

(** fast equality function on strings, doesn't use compare_val *)
val equal : t -> t -> bool

(** [is_empty s] returns [true] iff [s] is empty (i.e. its length is 0). *)
val is_empty : t -> bool

module Infix : sig
  val ( </> ) : t -> int * int -> t
end

val of_char : char -> t

val of_char_list : char list -> t

(** Operations for escaping and unescaping strings, with paramaterized escape and
    escapeworthy characters.  Escaping/unescaping using this module is more efficient than
    using Pcre. Benchmark code can be found in core/benchmarks/string_escaping.ml. *)
module Escaping : sig
  (** [escape_gen_exn escapeworthy_map escape_char] returns a function that will escape a
      string [s] as follows: if [(c1,c2)] is in [escapeworthy_map], then all occurences of
      [c1] are replaced by [escape_char] concatenated to [c2].

      Raises an exception if [escapeworthy_map] is not one-to-one.  If [escape_char] is
      not in [escapeworthy_map], then it will be escaped to itself.*)
  val escape_gen_exn
    :  escapeworthy_map:(char * char) list
    -> escape_char:char
    -> (string -> string) Staged.t

  val escape_gen
    :  escapeworthy_map:(char * char) list
    -> escape_char:char
    -> (string -> string) Or_error.t

  (** [escape ~escapeworthy ~escape_char s] is
      {[
        escape_gen_exn ~escapeworthy_map:(List.zip_exn escapeworthy escapeworthy)
          ~escape_char
      ]}.
      Duplicates and [escape_char] will be removed from [escapeworthy].  So, no
      exception will be raised *)
  val escape : escapeworthy:char list -> escape_char:char -> (string -> string) Staged.t

  (** [unescape_gen_exn] is the inverse operation of [escape_gen_exn]. That is,
      {[
      let escape = Staged.unstage (escape_gen_exn ~escapeworthy_map ~escape_char) in
      let unescape = Staged.unstage (unescape_gen_exn ~escapeworthy_map ~escape_char) in
      assert (s = unescape (escape s))
      ]}
      always succeed when ~escapeworthy_map is not causing exceptions. *)
  val unescape_gen_exn
    :  escapeworthy_map:(char * char) list
    -> escape_char:char
    -> (string -> string) Staged.t

  val unescape_gen
    :  escapeworthy_map:(char * char) list
    -> escape_char:char
    -> (string -> string) Or_error.t

  (** [unescape ~escape_char] is defined as [unescape_gen_exn ~map:\[\] ~escape_char] *)
  val unescape : escape_char:char -> (string -> string) Staged.t

  (** Any char in an escaped string is either escaping, escaped or literal. For example,
      for escaped string "0_a0__0" with escape_char as '_', pos 1 and 4 are escaping, 2
      and 5 are escaped, and the rest are literal

      [is_char_escaping s ~escape_char pos] return true if the char at [pos] is escaping,
      false otherwise. *)
  val is_char_escaping : string -> escape_char:char -> int -> bool

  (** [is_char_escaped s ~escape_char pos] return true if the char at [pos] is escaped,
      false otherwise. *)
  val is_char_escaped : string -> escape_char:char -> int -> bool

  (** [is_literal s ~escape_char pos] return true if the char at [pos] is not escaped or
      escaping. *)
  val is_char_literal : string -> escape_char:char -> int -> bool

  (** [index s ~escape_char char] find the first literal (not escaped) instance of
      char in s starting from 0. *)
  val index     : string -> escape_char:char -> char -> int option
  val index_exn : string -> escape_char:char -> char -> int

  (** [rindex s ~escape_char char] find the first literal (not escaped) instance of
      char in s starting from the end of s and proceeding towards 0. *)
  val rindex     : string -> escape_char:char -> char -> int option
  val rindex_exn : string -> escape_char:char -> char -> int

  (** [index_from s ~escape_char pos char] find the first literal (not escaped)
      instance of char in s starting from pos and proceeding towards the end of s. *)
  val index_from     : string -> escape_char:char -> int -> char -> int option
  val index_from_exn : string -> escape_char:char -> int -> char -> int

  (** [rindex_from s ~escape_char pos char] find the first literal (not escaped)
      instance of char in s starting from pos and towards 0. *)
  val rindex_from     : string -> escape_char:char -> int -> char -> int option
  val rindex_from_exn : string -> escape_char:char -> int -> char -> int

  (** [split s ~escape_char ~on] @return a list of substrings of [s] that are separated by
      literal versions of [on].  Consecutive [on] characters will cause multiple empty
      strings in the result.  Splitting the empty string returns a list of the empty
      string, not the empty list.

      e.g. split ~escape_char:'_' ~on:',' "foo,bar_,baz" = ["foo"; "bar_,baz"] *)
  val split : string -> on:char -> escape_char:char -> string list

  (** [split_on_chars s ~on] @return a list of all substrings of [s] that are separated by
      one of the literal chars from [on].  [on] are not grouped.  So a grouping of [on] in
      the source string will produce multiple empty string splits in the result.

      e.g. split_on_chars ~escape_char:'_' ~on:[',';'|'] "foo_|bar,baz|0" ->
      ["foo_|bar"; "baz"; "0"] *)
  val split_on_chars : string -> on:char list -> escape_char:char -> string list

  (* [lsplit2 s on escape_char] splits s into a pair on the first literal instance
     of [on] (meaning the first unescaped instance) starting from the left. *)
  val lsplit2     : string -> on:char -> escape_char:char -> (string * string) option
  val lsplit2_exn : string -> on:char -> escape_char:char -> (string * string)

  (* [rsplit2 s on escape_char] splits [s] into a pair on the first literal instance
     of [on] (meaning the first unescaped instance) starting from the right. *)
  val rsplit2     : string -> on:char -> escape_char:char -> (string * string) option
  val rsplit2_exn : string -> on:char -> escape_char:char -> (string * string)
end


external unsafe_get : string -> int -> char         = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
