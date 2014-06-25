module Array = Caml.ArrayLabels
module Char = Core_char
module String = Caml.StringLabels
module List = Core_list
open Typerep_lib.Std
open Sexplib.Std
open Bin_prot.Std
open Result.Export
open Staged

let phys_equal = (==)

let invalid_argf = Core_printf.invalid_argf

let failwiths = Error.failwiths

module T = struct
  type t = string with sexp, bin_io, typerep
  let compare = String.compare

  (* = on two strings avoids calling compare_val, which is what happens
     with String.compare *)
  let equal (x : string) y = x = y
end

include T

type elt = char

let max_length = Caml.Sys.max_string_length

(* Standard functions *)
let capitalize = String.capitalize
let concat ?(sep="") l = String.concat ~sep l
let copy = String.copy
let escaped = String.escaped
let fill = String.fill
let index_exn = String.index
let index_from_exn = String.index_from
let lowercase = String.lowercase
let make = String.make
let rindex_exn = String.rindex
let rindex_from_exn = String.rindex_from
let uncapitalize = String.uncapitalize
let uppercase = String.uppercase
external create : int -> string = "caml_create_string"
external get        : string -> int -> char = "%string_safe_get"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external length : string -> int = "%string_length"
external set        : string -> int -> char -> unit = "%string_safe_set"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"

include
  Blit.Make
    (struct
      type t = char
      let equal = (=)
      let of_bool b = if b then 'a' else 'b'
    end)
    (struct
      type nonrec t = t with sexp_of
      let create ~len = create len
      let length = length
      let get = get
      let set = set
      let unsafe_blit = String.unsafe_blit
    end)
;;

let contains ?pos ?len t char =
  let (pos, len) =
    Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(length t)
  in
  let last = pos + len in
  let rec loop i = i < last && (t.[i] = char || loop (i + 1)) in
  loop pos
;;

TEST = contains "" 'a' = false
TEST = contains "a" 'a' = true
TEST = contains "a" 'b' = false
TEST = contains "ab" 'a' = true
TEST = contains "ab" 'b' = true
TEST = contains "ab" 'c' = false
TEST = contains "abcd" 'b' ~pos:1 ~len:0 = false
TEST = contains "abcd" 'b' ~pos:1 ~len:1 = true
TEST = contains "abcd" 'c' ~pos:1 ~len:2 = true
TEST = contains "abcd" 'd' ~pos:1 ~len:2 = false
TEST = contains "abcd" 'd' ~pos:1 = true
TEST = contains "abcd" 'a' ~pos:1 = false

let is_empty t = length t = 0

let index t char =
  try Some (index_exn t char)
  with Not_found -> None

let rindex t char =
  try Some (rindex_exn t char)
  with Not_found -> None

let index_from t pos char =
  try Some (index_from_exn t pos char)
  with Not_found -> None

let rindex_from t pos char =
  try Some (rindex_from_exn t pos char)
  with Not_found -> None

module Search_pattern = struct

  type t = string * int array with sexp_of

  (* Find max number of matched characters at [next_text_char], given the current
     [matched_chars]. Try to extend the current match, if chars don't match, try to match
     fewer chars. If chars match then extend the match. *)
  let kmp_internal_loop ~matched_chars ~next_text_char ~pattern ~kmp_arr =
    let matched_chars = ref matched_chars in
    while !matched_chars > 0 && next_text_char <> unsafe_get pattern !matched_chars do
      matched_chars := Core_array.unsafe_get kmp_arr (!matched_chars - 1)
    done;
    if next_text_char = unsafe_get pattern !matched_chars then
      matched_chars := !matched_chars + 1;
    !matched_chars
  ;;

  (* Classic KMP pre-processing of the pattern: build the int array, which, for each i,
     contains the length of the longest non-trivial prefix of s which is equal to a suffix
     ending at s.[i] *)
  let create pattern =
    let n = length pattern in
    let kmp_arr = Core_array.create ~len:n (-1) in
    if n > 0 then begin
      Core_array.unsafe_set kmp_arr 0 0;
      let matched_chars = ref 0 in
      for i = 1 to n - 1 do
        matched_chars :=
          kmp_internal_loop
            ~matched_chars:!matched_chars
            ~next_text_char:(unsafe_get pattern i)
            ~pattern
            ~kmp_arr;
        Core_array.unsafe_set kmp_arr i !matched_chars
      done
    end;
    (pattern, kmp_arr)
  ;;

  TEST_MODULE "Search_pattern.create" = struct
    let prefix s n = sub s ~pos:0 ~len:n
    let suffix s n = sub s ~pos:(length s - n) ~len:n

    let slow_create pattern =
      (* Compute the longest prefix-suffix array from definition, O(n^3) *)
      let n = length pattern in
      let kmp_arr = Core_array.create ~len:n (-1) in
      for i = 0 to n - 1 do
        let x = prefix pattern (i + 1) in
        for j = 0 to i do
          if prefix x j = suffix x j then
            kmp_arr.(i) <- j
        done
      done;
      (pattern, kmp_arr)
    ;;

    let test_both (s, a) = create s = (s, a) && slow_create s = (s, a)
    let cmp_both s = create s = slow_create s

    TEST = test_both ("", [| |])
    TEST = test_both ("ababab", [|0; 0; 1; 2; 3; 4|])
    TEST = test_both ("abaCabaD", [|0; 0; 1; 0; 1; 2; 3; 0|])
    TEST = test_both ("abaCabaDabaCabaCabaDabaCabaEabab",
                           [|0; 0; 1; 0; 1; 2; 3; 0; 1; 2; 3; 4; 5; 6; 7; 4; 5; 6; 7; 8;
                             9; 10; 11; 12; 13; 14; 15; 0; 1; 2; 3; 2|])

    let rec x k =
      if k < 0 then "" else
        let b = x (k - 1) in
        b ^ (make 1 (Caml.Char.unsafe_chr (65 + k))) ^ b
    ;;

    TEST = cmp_both (x 10)
    TEST = cmp_both ((x 5) ^ "E" ^ (x 4) ^ "D" ^ (x 3) ^ "B" ^ (x 2) ^ "C" ^ (x 3))
  end

  (* Classic KMP: use the pre-processed pattern to optimize look-behinds on non-matches.
     We return int to avoid allocation in [index_exn]. -1 means no match. *)
  let index_internal ?(pos=0) (pattern, kmp_arr) ~in_:text =
    if pos < 0 || pos > length text - length pattern then
      -1
    else begin
      let j = ref pos in
      let matched_chars = ref 0 in
      let k = length pattern in
      let n = length text in
      while !j < n && !matched_chars < k do
        let next_text_char = unsafe_get text !j in
        matched_chars :=
          kmp_internal_loop
            ~matched_chars:!matched_chars
            ~next_text_char
            ~pattern
            ~kmp_arr;
        j := !j + 1
      done;
      if !matched_chars = k then
        !j - k
      else
        -1
    end
  ;;

  let index ?pos t ~in_ =
    let p = index_internal ?pos t ~in_ in
    if p < 0 then
      None
    else
      Some p
  ;;

  TEST = index (create "") ~in_:"abababac" = Some 0
  TEST = index ~pos:(-1) (create "") ~in_:"abababac" = None
  TEST = index ~pos:1 (create "") ~in_:"abababac" = Some 1
  TEST = index ~pos:7 (create "") ~in_:"abababac" = Some 7
  TEST = index ~pos:8 (create "") ~in_:"abababac" = Some 8
  TEST = index ~pos:9 (create "") ~in_:"abababac" = None
  TEST = index (create "abababaca") ~in_:"abababac" = None
  TEST = index (create "abababac") ~in_:"abababac" = Some 0
  TEST = index ~pos:0 (create "abababac") ~in_:"abababac" = Some 0
  TEST = index (create "abac") ~in_:"abababac" = Some 4
  TEST = index ~pos:4 (create "abac") ~in_:"abababac" = Some 4
  TEST = index ~pos:5 (create "abac") ~in_:"abababac" = None
  TEST = index ~pos:5 (create "abac") ~in_:"abababaca" = None
  TEST = index ~pos:5 (create "baca") ~in_:"abababaca" = Some 5
  TEST = index ~pos:(-1) (create "a") ~in_:"abc" = None
  TEST = index ~pos:2 (create "a") ~in_:"abc" = None
  TEST = index ~pos:2 (create "c") ~in_:"abc" = Some 2
  TEST = index ~pos:3 (create "c") ~in_:"abc" = None

  let index_exn ?pos t ~in_ =
    let p = index_internal ?pos t ~in_ in
    if p >= 0 then
      p
    else
      failwiths "Substring not found" (fst t) sexp_of_string
  ;;

  let index_all (pattern, kmp_arr) ~may_overlap ~in_:text =
    if length pattern = 0 then
      List.init (1 + length text) ~f:Fn.id
    else begin
      let matched_chars = ref 0 in
      let k = length pattern in
      let n = length text in
      let found = ref [] in
      for j = 0 to n do
        if !matched_chars = k then begin
          found := (j - k)::!found;
          (* we just found a match in the previous iteration *)
          match may_overlap with
          | true -> matched_chars := Core_array.unsafe_get kmp_arr (k - 1)
          | false -> matched_chars := 0
        end;
        if j < n then begin
          let next_text_char = unsafe_get text j in
          matched_chars :=
            kmp_internal_loop
              ~matched_chars:!matched_chars
              ~next_text_char
              ~pattern
              ~kmp_arr
        end
      done;
      Core_list.rev !found
    end
  ;;

  TEST = index_all (create "") ~may_overlap:false ~in_:"abcd" = [0; 1; 2; 3; 4]
  TEST = index_all (create "") ~may_overlap:true ~in_:"abcd" = [0; 1; 2; 3; 4]
  TEST = index_all (create "abab") ~may_overlap:false ~in_:"abababab" = [0; 4]
  TEST = index_all (create "abab") ~may_overlap:true  ~in_:"abababab" = [0; 2; 4]
  TEST = index_all (create "abab") ~may_overlap:false ~in_:"ababababab" = [0; 4]
  TEST = index_all (create "abab") ~may_overlap:true  ~in_:"ababababab" = [0; 2; 4; 6]
  TEST = index_all (create "aaa") ~may_overlap:false ~in_:"aaaaBaaaaaa" = [0; 5; 8]
  TEST = index_all (create "aaa") ~may_overlap:true  ~in_:"aaaaBaaaaaa" = [0; 1; 5; 6; 7; 8]

  let replace_first ?pos t ~in_:s ~with_ =
    match index ?pos t ~in_:s with
    | None -> s
    | Some i ->
      let len_s = length s in
      let len_t = length (fst t) in
      let len_with = length with_ in
      let dst = make (len_s + len_with - len_t) ' ' in
      blit ~src:s ~src_pos:0 ~dst ~dst_pos:0 ~len:i;
      blit ~src:with_ ~src_pos:0 ~dst ~dst_pos:i ~len:len_with;
      blit ~src:s ~src_pos:(i + len_t) ~dst ~dst_pos:(i + len_with) ~len:(len_s - i - len_t);
      dst
  ;;

  TEST = replace_first (create "abab") ~in_:"abababab" ~with_:"" = "abab"
  TEST = replace_first (create "abab") ~in_:"abacabab" ~with_:"" = "abac"
  TEST = replace_first (create "abab") ~in_:"ababacab" ~with_:"A" = "Aacab"
  TEST = replace_first (create "abab") ~in_:"acabababab" ~with_:"A" = "acAabab"
  TEST = replace_first (create "ababab") ~in_:"acabababab" ~with_:"A" = "acAab"
  TEST = replace_first (create "abab") ~in_:"abababab" ~with_:"abababab" = "abababababab"


  let replace_all t ~in_:s ~with_ =
    let matches = index_all t ~may_overlap:false ~in_:s in
    match matches with
    | [] -> s
    | _::_ ->
      let len_s = length s in
      let len_t = length (fst t) in
      let len_with = length with_ in
      let num_matches = Core_list.length matches in
      let dst = make (len_s + (len_with - len_t) * num_matches) ' ' in
      let next_dst_pos = ref 0 in
      let next_src_pos = ref 0 in
      List.iter matches ~f:(fun i ->
        let len = i - !next_src_pos in
        blit ~src:s ~src_pos:!next_src_pos ~dst ~dst_pos:!next_dst_pos ~len;
        blit ~src:with_ ~src_pos:0 ~dst ~dst_pos:(!next_dst_pos + len) ~len:len_with;
        next_dst_pos := !next_dst_pos + len + len_with;
        next_src_pos := !next_src_pos + len + len_t;
      );
      blit ~src:s ~src_pos:!next_src_pos ~dst ~dst_pos:!next_dst_pos
        ~len:(len_s - !next_src_pos);
      dst
  ;;

  TEST = replace_all (create "abab") ~in_:"abababab" ~with_:"" = ""
  TEST = replace_all (create "abab") ~in_:"abacabab" ~with_:"" = "abac"
  TEST = replace_all (create "abab") ~in_:"acabababab" ~with_:"A" = "acAA"
  TEST = replace_all (create "ababab") ~in_:"acabababab" ~with_:"A" = "acAab"
  TEST = replace_all (create "abaC") ~in_:"abaCabaDCababaCabaCaba" ~with_:"x" = "xabaDCabxxaba"
  TEST = replace_all (create "a") ~in_:"aa" ~with_:"aaa" = "aaaaaa"
  TEST = replace_all (create "") ~in_:"abcdeefff" ~with_:"X1" = "X1aX1bX1cX1dX1eX1eX1fX1fX1fX1"

  (* a doc comment in core_string.mli gives this as an example *)
  TEST = replace_all (create "bc") ~in_:"aabbcc" ~with_:"cb" = "aabcbc"
end

let substr_index ?pos t ~pattern =
  Search_pattern.index ?pos (Search_pattern.create pattern) ~in_:t
;;

let substr_index_exn ?pos t ~pattern =
  Search_pattern.index_exn ?pos (Search_pattern.create pattern) ~in_:t
;;

let substr_index_all t ~may_overlap ~pattern =
  Search_pattern.index_all (Search_pattern.create pattern) ~may_overlap ~in_:t
;;

let substr_replace_first ?pos t ~pattern =
  Search_pattern.replace_first ?pos (Search_pattern.create pattern) ~in_:t
;;

let substr_replace_all t ~pattern =
  Search_pattern.replace_all (Search_pattern.create pattern) ~in_:t
;;

let id x = x
let of_string = id
let to_string = id

let iter t ~f = String.iter t ~f

let init n ~f =
  if n < 0 then invalid_argf "String.init %d" n ();
  let t = create n in
  for i = 0 to n - 1 do
    t.[i] <- f i;
  done;
  t
;;

(** See {!Core_array.normalize} for the following 4 functions. *)
let normalize t i =
  Ordered_collection_common.normalize ~length_fun:String.length t i
let slice t start stop =
  Ordered_collection_common.slice ~length_fun:String.length ~sub_fun:String.sub
    t start stop


(*TEST = slice "hey" 0 0 = ""*) (* This is what I would expect *)
TEST = slice "hey" 0 0 = "hey" (* But this is what we get! *)

TEST = slice "hey" 0 1 = "h"
TEST = slice "hey" 0 2 = "he"
TEST = slice "hey" 0 3 = "hey"
TEST = slice "hey" 1 1 = ""
TEST = slice "hey" 1 2 = "e"
TEST = slice "hey" 1 3 = "ey"
TEST = slice "hey" 2 2 = ""
TEST = slice "hey" 2 3 = "y"
TEST = slice "hey" 3 3 = ""

let nget x i =
  x.[normalize x i]
let nset x i v =
  x.[normalize x i] <- v

let invalid_argf = Core_printf.invalid_argf

let to_list s =
  let rec loop acc i =
    if i < 0 then
      acc
    else
      loop (s.[i] :: acc) (i-1)
  in
  loop [] (String.length s - 1)

let to_list_rev s =
  let len = String.length s in
  let rec loop acc i =
    if i = len then
      acc
    else
      loop (s.[i] :: acc) (i+1)
  in
  loop [] 0

let rev t =
  let len = String.length t in
  let res = String.create len in
  for i = 0 to len - 1 do
    unsafe_set res i (unsafe_get t (len - 1 - i))
  done;
  res
;;

TEST = rev "" = "";;
TEST = rev "a" = "a";;
TEST = rev "ab" = "ba";;
TEST = rev "abc" = "cba";;

(** Efficient string splitting *)

let lsplit2_exn line ~on:delim =
  let pos = String.index line delim in
  (String.sub line ~pos:0 ~len:pos,
   String.sub line ~pos:(pos+1) ~len:(String.length line - pos - 1)
  )

let rsplit2_exn line ~on:delim =
  let pos = String.rindex line delim in
  (String.sub line ~pos:0 ~len:pos,
   String.sub line ~pos:(pos+1) ~len:(String.length line - pos - 1)
  )

let lsplit2 line ~on =
  try Some (lsplit2_exn line ~on) with Not_found -> None

let rsplit2 line ~on =
  try Some (rsplit2_exn line ~on) with Not_found -> None

let rec char_list_mem l (c:char) =
  match l with
  | [] -> false
  | hd::tl -> hd = c || char_list_mem tl c

let split_gen str ~on =
  let is_delim =
    match on with
    | `char c' -> (fun c -> c = c')
    | `char_list l -> (fun c -> char_list_mem l c)
  in
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      String.sub str ~pos:0 ~len:last_pos :: acc
    else
      if is_delim str.[pos] then
        let pos1 = pos + 1 in
        let sub_str = String.sub str ~pos:pos1 ~len:(last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)
;;

let split str ~on = split_gen str ~on:(`char on) ;;

let split_on_chars str ~on:chars =
  split_gen str ~on:(`char_list chars)
;;

let split_lines =
  let back_up_at_newline ~t ~pos ~eol =
    pos := !pos - (if !pos > 0 && t.[!pos - 1] = '\r' then 2 else 1);
    eol := !pos + 1;
  in
  fun t ->
    let n = length t in
    if n = 0
    then []
    else
      (* Invariant: [-1 <= pos < eol]. *)
      let pos = ref (n - 1) in
      let eol = ref n in
      let ac = ref [] in
      (* We treat the end of the string specially, because if the string ends with a
         newline, we don't want an extra empty string at the end of the output. *)
      if t.[!pos] = '\n' then back_up_at_newline ~t ~pos ~eol;
      while !pos >= 0 do
        if t.[!pos] <> '\n'
        then decr pos
        else
          (* Becuase [pos < eol], we know that [start <= eol]. *)
          let start = !pos + 1 in
          ac := sub t ~pos:start ~len:(!eol - start) :: !ac;
          back_up_at_newline ~t ~pos ~eol
      done;
      sub t ~pos:0 ~len:!eol :: !ac
;;

TEST_UNIT =
  List.iter ~f:(fun (t, expect) ->
    let actual = split_lines t in
    if actual <> expect
    then failwiths "split_lines bug" (t, `actual actual , `expect expect)
           <:sexp_of< t * [ `actual of t list ] * [ `expect of t list ] >>)
    [ ""             , [];
      "\n"           , [""];
      "a"            , ["a"];
      "a\n"          , ["a"];
      "a\nb"         , ["a"; "b"];
      "a\nb\n"       , ["a"; "b"];
      "a\n\n"        , ["a"; "" ];
      "a\n\nb"       , ["a"; "" ; "b"];
    ]
;;

TEST_UNIT =
  let lines = [ ""; "a"; "bc" ] in
  let newlines = [ "\n"; "\r\n" ] in
  let rec loop n expect to_concat =
    if n = 0 then begin
      let input = concat to_concat in
      let actual = Or_error.try_with (fun () -> split_lines input) in
      if actual <> Ok expect
      then failwiths "split_lines bug" (input, `actual actual , `expect expect)
             <:sexp_of< t * [ `actual of t list Or_error.t ] * [ `expect of t list ] >>
    end else begin
      loop (n - 1) expect to_concat;
      List.iter lines ~f:(fun t ->
        let loop to_concat = loop (n - 1) (t :: expect) (t :: to_concat) in
        if not (is_empty t) && List.is_empty to_concat then loop [];
        List.iter newlines ~f:(fun newline -> loop (newline :: to_concat)));
    end
  in
  loop 3 [] [];
;;

(* [is_suffix s ~suff] returns [true] if the string [s] ends with the suffix [suff] *)
let is_suffix s ~suffix =
  let len_suff = String.length suffix in
  let len_s = String.length s in
  len_s >= len_suff
  && (let rec loop i =
        i = len_suff || (suffix.[len_suff - 1 - i] = s.[len_s - 1 - i] && loop (i + 1))
      in
      loop 0)

let is_prefix s ~prefix =
  let len_pref = String.length prefix in
  String.length s >= len_pref
  && (let rec loop i =
        i = len_pref || (prefix.[i] = s.[i] && loop (i + 1))
      in
      loop 0)
;;

let wrap_sub_n t n ~name ~pos ~len ~on_error =
  if n < 0 then
    invalid_arg (name ^ " expecting nonnegative argument")
  else
    try
      sub t ~pos ~len
    with _ ->
      on_error

let drop_prefix t n = wrap_sub_n ~name:"drop_prefix" t n ~pos:n ~len:(length t - n) ~on_error:""
let drop_suffix t n = wrap_sub_n ~name:"drop_suffix" t n ~pos:0 ~len:(length t - n) ~on_error:""
let prefix t n = wrap_sub_n ~name:"prefix" t n ~pos:0 ~len:n ~on_error:t
let suffix t n = wrap_sub_n ~name:"suffix" t n ~pos:(length t - n) ~len:n ~on_error:t

let lfindi ?(pos=0) t ~f =
  let n = length t in
  let rec loop i =
    if i = n then None
    else if f i t.[i] then Some i
    else loop (i + 1)
  in
  loop pos
;;

TEST = lfindi "bob" ~f:(fun _ c -> 'b' = c) = Some 0
TEST = lfindi ~pos:0 "bob" ~f:(fun _ c -> 'b' = c) = Some 0
TEST = lfindi ~pos:1 "bob" ~f:(fun _ c -> 'b' = c) = Some 2
TEST = lfindi "bob" ~f:(fun _ c -> 'x' = c) = None

let find t ~f =
  match lfindi t ~f:(fun _ c -> f c) with
  | None -> None | Some i -> Some t.[i]

let find_map t ~f =
  let n = length t in
  let rec loop i =
    if i = n then None
    else
      match f t.[i] with
      | None -> loop (i + 1)
      | Some _ as res -> res
  in
  loop 0
;;

TEST = find_map "fop" ~f:(fun c -> if c >= 'o' then Some c else None) = Some 'o'
TEST = find_map "bar" ~f:(fun _ -> None) = None
TEST = find_map "" ~f:(fun _ -> assert false) = None

let rfindi ?pos t ~f =
  let rec loop i =
    if i < 0 then None
    else begin
      if f i t.[i] then Some i
      else loop (i - 1)
    end
  in
  let pos =
    match pos with
    | Some pos -> pos
    | None -> length t - 1
  in
  loop pos
;;

TEST = rfindi "bob" ~f:(fun _ c -> 'b' = c) = Some 2
TEST = rfindi ~pos:2 "bob" ~f:(fun _ c -> 'b' = c) = Some 2
TEST = rfindi ~pos:1 "bob" ~f:(fun _ c -> 'b' = c) = Some 0
TEST = rfindi "bob" ~f:(fun _ c -> 'x' = c) = None

let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c))

let rstrip ?(drop=Char.is_whitespace) t =
  match last_non_drop t ~drop with
  | None -> ""
  | Some i ->
    if i = length t - 1
    then t
    else prefix t (i + 1)
;;

let first_non_drop ~drop t = lfindi t ~f:(fun _ c -> not (drop c))

let lstrip ?(drop=Char.is_whitespace) t =
  match first_non_drop t ~drop with
  | None -> ""
  | Some 0 -> t
  | Some n -> drop_prefix t n
;;

(* [strip t] could be implemented as [lstrip (rstrip t)].  The implementation
   below saves (at least) a factor of two allocation, by only allocating the
   final result.  This also saves some amount of time. *)
let strip ?(drop=Char.is_whitespace) t =
  let length = length t in
  if length = 0 || not (drop t.[0] || drop t.[length - 1])
  then t
  else
    match first_non_drop t ~drop with
    | None -> ""
    | Some first ->
        match last_non_drop t ~drop with
        | None -> assert false
        | Some last -> sub t ~pos:first ~len:(last - first + 1)
;;

TEST = strip " foo bar \n" = "foo bar"
TEST = strip ~drop:(fun c -> c = '"') "\" foo bar " = " foo bar "
TEST = strip ~drop:(fun c -> c = '"') " \" foo bar " = " \" foo bar "

let mapi t ~f =
  let l = String.length t in
  let t' = String.create l in
  for i = 0 to l - 1 do
    t'.[i] <- f i t.[i]
  done;
  t'

(* repeated code to avoid requiring an extra allocation for a closure on each call. *)
let map t ~f =
  let l = String.length t in
  let t' = String.create l in
  for i = 0 to l - 1 do
    t'.[i] <- f t.[i]
  done;
  t'

let to_array s = Array.init (String.length s) ~f:(fun i -> s.[i])

let tr ~target ~replacement s = map ~f:(fun c -> if c = target then replacement else c) s

let tr_inplace ~target ~replacement s = (* destructive version of tr *)
  for i = 0 to String.length s - 1 do
    if s.[i] = target then s.[i] <- replacement
  done

let exists s ~f =
  let length = length s in
  let rec loop i = i < length && (f s.[i] || loop (i + 1)) in
  loop 0
;;

TEST = false = exists ""    ~f:(fun _ -> assert false)
TEST = false = exists "abc" ~f:(Fn.const false)
TEST = true  = exists "abc" ~f:(Fn.const true)
TEST = true  = exists "abc" ~f:(function
    'a' -> false | 'b' -> true | _ -> assert false)

let for_all s ~f =
  let length = length s in
  let rec loop i = i = length || (f s.[i] && loop (i + 1)) in
  loop 0
;;

TEST = true  = for_all ""    ~f:(fun _ -> assert false)
TEST = true  = for_all "abc" ~f:(Fn.const true)
TEST = false = for_all "abc" ~f:(Fn.const false)
TEST = false = for_all "abc" ~f:(function
    'a' -> true | 'b' -> false | _ -> assert false)

let fold t ~init ~f =
  let n = length t in
  let rec loop i ac = if i = n then ac else loop (i + 1) (f ac t.[i]) in
  loop 0 init
;;

let foldi t ~init ~f =
  let n = length t in
  let rec loop i ac = if i = n then ac else loop (i + 1) (f i ac t.[i]) in
  loop 0 init
;;

TEST = (foldi "hello" ~init:[] ~f:(fun i acc ch -> (i,ch)::acc)
        = List.rev [0,'h';1,'e';2,'l';3,'l';4,'o'])

let count t ~f = Container.fold_count fold t ~f

let mem ?(equal = Char.(=)) t c =
  let rec loop i = i < length t && (equal c t.[i] || loop (i + 1)) in
  loop 0
;;

let concat_array ?sep ar = concat ?sep (Array.to_list ar)

let concat_map ?sep s ~f = concat_array ?sep (Array.map (to_array s) ~f)

(* [filter t f] is implemented by the following algorithm.

   Let [n = length t].

   1. Find the lowest [i] such that [not (f t.[i])].

   2. If there is no such [i], then return [t].

   3. If there is such an [i], allocate a string, [out], to hold the result.  [out] has
   length [n - 1], which is the maximum possible output size given that there is at least
   one character not satisfying [f].

   4. Copy characters at indices 0 ... [i - 1] from [t] to [out].

   5. Walk through characters at indices [i+1] ... [n-1] of [t], copying those that
   satisfy [f] from [t] to [out].

   6. If we completely filled [out], then return it.  If not, return the prefix of [out]
   that we did fill in.

   This algorithm has the property that it doesn't allocate a new string if there's
   nothing to filter, which is a common case. *)
let filter t ~f =
  let n = length t in
  let i = ref 0 in
  while !i < n && f t.[!i]; do
    incr i
  done;
  if !i = n then
    t
  else begin
    let out = make (n - 1) ' ' in
    blit ~src:t ~src_pos:0 ~dst:out ~dst_pos:0 ~len:!i;
    let out_pos = ref !i in
    incr i;
    while !i < n; do
      let c = t.[!i] in
      if f c then (out.[!out_pos] <- c; incr out_pos);
      incr i
    done;
    if !out_pos = n - 1 then
      out
    else
      sub out ~pos:0 ~len:!out_pos
  end
;;

TEST = filter "hello" ~f:(fun c -> c <> 'h') = "ello"
TEST = filter "hello" ~f:(fun c -> c <> 'l') = "heo"
TEST = filter "hello" ~f:(fun _ -> false) = ""
TEST = filter "hello" ~f:(fun _ -> true) = "hello"
TEST = let s = "hello" in (filter s ~f:(fun _ -> true)) == s
TEST_UNIT =
  let s = "abc" in
  let r = ref 0 in
  assert (phys_equal s (filter s ~f:(fun _ -> incr r; true)));
  assert (!r = String.length s);
;;

let chop_prefix s ~prefix =
  if is_prefix s ~prefix then
    Some (drop_prefix s (String.length prefix))
  else
    None

let chop_prefix_exn s ~prefix =
  match chop_prefix s ~prefix with
  | Some str -> str
  | None ->
    raise (Invalid_argument
             (Printf.sprintf "Core_string.chop_prefix_exn %S %S" s prefix))

let chop_suffix s ~suffix =
  if is_suffix s ~suffix then
    Some (drop_suffix s (String.length suffix))
  else
    None

let chop_suffix_exn s ~suffix =
  match chop_suffix s ~suffix with
  | Some str -> str
  | None ->
    raise (Invalid_argument
             (Printf.sprintf "Core_string.chop_suffix_exn %S %S" s suffix))

(* There used to be a custom implementation that was faster for very short strings
   (peaking at 40% faster for 4-6 char long strings).
   This new function is around 20% faster than the default hash function, but slower
   than the previous custom implementation. However, the new OCaml function is well
   behaved, and this implementation is less likely to diverge from the default OCaml
   implementation does, which is a desirable property. (The only way to avoid the
   divergence is to expose the macro redefined in hash_stubs.c in the hash.h header of
   the OCaml compiler.) *)
module Hash = struct
  external hash : string -> int = "caml_hash_string" "noalloc"

  TEST_UNIT =
    List.iter ~f:(fun string -> assert (hash string = Caml.Hashtbl.hash string))
      [ "Oh Gloria inmarcesible! Oh jubilo inmortal!"
      ; "Oh say can you see, by the dawn's early light"
      ]
  ;;

end

module Infix = struct
  let ( </> ) str (start,stop) = slice str start stop
end

include (Hashable.Make_binable (struct
  include T
  include Hash
end) : Hashable.S_binable with type t := t)

(* [include Hash] to make the [external] version override the [hash] from
   [Hashable.Make_binable], so that we get a little bit of a speedup by exposing it as
   external in the mli. *)
include Hash


include Comparable.Map_and_set_binable (T)
include Comparable.Validate (T)

(* for interactive top-levels -- modules deriving from String should have String's pretty
   printer. *)
let pp = Format.pp_print_string

(* fast version, if we ever need it:
  let concat_array ~sep ar =
  let ar_len = Array.length ar in
  if ar_len = 0 then ""
  else
    let sep_len = String.length sep in
    let res_len_ref = ref (sep_len * (ar_len - 1)) in
    for i = 0 to ar_len - 1 do
      res_len_ref := !res_len_ref + String.length ar.(i)
    done;
    let res = String.create !res_len_ref in
    let str_0 = ar.(0) in
    let len_0 = String.length str_0 in
    String.blit ~src:str_0 ~src_pos:0 ~dst:res ~dst_pos:0 ~len:len_0;
    let pos_ref = ref len_0 in
    for i = 1 to ar_len - 1 do
      let pos = !pos_ref in
      String.blit ~src:sep ~src_pos:0 ~dst:res ~dst_pos:pos ~len:sep_len;
      let new_pos = pos + sep_len in
      let str_i = ar.(i) in
      let len_i = String.length str_i in
      String.blit ~src:str_i ~src_pos:0 ~dst:res ~dst_pos:new_pos ~len:len_i;
      pos_ref := new_pos + len_i
    done;
    res
  *)

let of_char c = String.make 1 c

let of_char_list l =
  let t = create (List.length l) in
  List.iteri l ~f:(fun i c -> t.[i] <- c);
  t

TEST = of_char_list ['a';'b';'c'] = "abc"
TEST = of_char_list [] = ""

module Escaping = struct

  (* If this is changed, make sure to update [escape], which attempts to ensure all the
     invariants checked here.  *)
  let build_and_validate_escapeworthy_map escapeworthy_map escape_char func =
    let escapeworthy_map =
      if List.Assoc.mem escapeworthy_map escape_char
      then escapeworthy_map
      else (escape_char, escape_char) :: escapeworthy_map
    in
    let arr = Array.create 256 (-1) in
    let rec loop vals = function
      | [] -> Ok arr
      | (c_from, c_to) :: l ->
        let k, v = match func with
          | `Escape -> Char.to_int c_from, c_to
          | `Unescape -> Char.to_int c_to, c_from
        in
        if arr.(k) <> -1 || Char.Set.mem vals v then
          Or_error.error "escapeworthy_map not one-to-one"
            (c_from, c_to, escapeworthy_map)
            (<:sexp_of< char * char * (char * char) list >>)
        else (arr.(k) <- Char.to_int v; loop (Char.Set.add vals v) l)
    in
    loop Char.Set.empty escapeworthy_map
  ;;

  let escape_gen ~escapeworthy_map ~escape_char =
    match
      build_and_validate_escapeworthy_map escapeworthy_map escape_char `Escape
    with
    | Error _ as x -> x
    | Ok escapeworthy ->
      Ok (fun src ->
        (* calculate a list of (index of char to escape * escaped char) first, the order
           is from tail to head *)
        let to_escape_len = ref 0 in
        let to_escape =
          foldi src ~init:[] ~f:(fun i acc c ->
            match escapeworthy.(Char.to_int c) with
            | -1 -> acc
            | n ->
              (* (index of char to escape * escaped char) *)
              incr to_escape_len;
              (i, Char.unsafe_of_int n) :: acc)
        in
        match to_escape with
        | [] -> src
        | _ ->
          (* [to_escape] divide [src] to [List.length to_escape + 1] pieces separated by
             the chars to escape.

             Lets take
             {[
             escape_gen_exn
             ~escapeworthy_map:[('a', 'A'); ('b', 'B'); ('c', 'C')]
             ~escape_char:'_'
             ]}
             for example, and assume the string to escape is

             "000a111b222c333"

             then [to_escape] is [(11, 'C'); (7, 'B'); (3, 'A')].

             Then we create a [dst] of length [String.length src + 3] to store the
             result, copy piece "333" to [dst] directly, then copy '_' and 'C' to [dst];
             then move on to next; after 3 iterations, copy piece "000" and we are done.

             Finally the result will be

             "000_A111_B222_C333"
          *)
          let src_len = String.length src in
          let dst_len = src_len + !to_escape_len in
          let dst = String.create dst_len in
          let rec loop last_idx last_dst_pos = function
            | [] ->
              (* copy "000" at last *)
              blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:last_idx
            | (idx, escaped_char) :: to_escape -> (*[idx] = the char to escape*)
              (* take first iteration for example *)
              (* calculate length of "333", minus 1 because we don't copy 'c' *)
              let len = last_idx - idx - 1 in
              (* set the dst_pos to copy to *)
              let dst_pos = last_dst_pos - len in
              (* copy "333", set [src_pos] to [idx + 1] to skip 'c' *)
              blit ~src ~src_pos:(idx + 1) ~dst ~dst_pos ~len;
              (* backoff [dst_pos] by 2 to copy '_' and 'C' *)
              let dst_pos = dst_pos - 2 in
              dst.[dst_pos] <- escape_char;
              dst.[dst_pos + 1] <- escaped_char;
              loop idx dst_pos to_escape
          in
          (* set [last_dst_pos] and [last_idx] to length of [dst] and [src] first *)
          loop src_len dst_len to_escape;
          dst
      )
  ;;

  let escape_gen_exn ~escapeworthy_map ~escape_char =
    Or_error.ok_exn (escape_gen ~escapeworthy_map ~escape_char) |! stage
  ;;

  TEST_MODULE "escape_gen" = struct
    let escape = unstage
      (escape_gen_exn
         ~escapeworthy_map:[('%','p');('^','c')] ~escape_char:'_')

    TEST = escape "" = ""
    TEST = escape "foo" = "foo"
    TEST = escape "_" = "__"
    TEST = escape "foo%bar" = "foo_pbar"
    TEST = escape "^foo%" = "_cfoo_p"

    let escape2 = unstage
      (escape_gen_exn
         ~escapeworthy_map:[('_','.');('%','p');('^','c')] ~escape_char:'_')

    TEST = escape2 "_." = "_.."
    TEST = escape2 "_" = "_."
    TEST = escape2 "foo%_bar" = "foo_p_.bar"
    TEST = escape2 "_foo%" = "_.foo_p"

    let checks_for_one_to_one escapeworthy_map =
      try
        let _escape = escape_gen_exn ~escapeworthy_map ~escape_char:'_' in
        false
      with _ -> true

    TEST = checks_for_one_to_one [('%','p');('^','c');('$','c')]
    TEST = checks_for_one_to_one [('%','p');('^','c');('%','d')]
  end

  let escape ~escapeworthy ~escape_char =
    (* For [escape_gen_exn], we don't know how to fix invalid escapeworthy_map so we have
       to raise exception; but in this case, we know how to fix duplicated elements in
       escapeworthy list, so we just fix it instead of raising exception to make this
       function easier to use.  *)
    let escapeworthy_map =
      List.map ~f:(fun c -> (c, c))
        (Char.Set.to_list (Char.Set.remove (Char.Set.of_list escapeworthy) escape_char))
    in
    escape_gen_exn ~escapeworthy_map ~escape_char
  ;;

  (* In an escaped string, any char is either `Escaping, `Escaped or `Literal. For
     example, the escape statuses of chars in string "a_a__" with escape_char = '_' are

       a : `Literal
       _ : `Escaping
       a : `Escaped
       _ : `Escaping
       _ : `Escaped

     [update_escape_status str ~escape_char i previous_status] gets escape status of
     str.[i] basing on escape status of str.[i - 1]
  *)
  let update_escape_status str ~escape_char i = function
    | `Escaping -> `Escaped
    | `Literal
    | `Escaped -> if str.[i] = escape_char then `Escaping else `Literal
  ;;

  let unescape_gen ~escapeworthy_map ~escape_char =
    match
      build_and_validate_escapeworthy_map escapeworthy_map escape_char `Unescape
    with
    | Error _ as x -> x
    | Ok escapeworthy ->
      Ok (fun src ->
        (* Continue the example in [escape_gen_exn], now we unescape

           "000_A111_B222_C333"

           back to

           "000a111b222c333"

           Then [to_unescape] is [14; 9; 4], which is indexes of '_'s.

           Then we create a string [dst] to store the result, copy "333" to it, then copy
           'c', then move on to next iteration. After 3 iterations copy "000" and we are
           done.  *)
        (* indexes of escape chars *)
        let to_unescape =
          let rec loop i status acc =
            if i >= String.length src then acc
            else
              let status = update_escape_status src ~escape_char i status in
              loop (i + 1) status (if status = `Escaping then i :: acc else acc)
          in
          loop 0 `Literal []
        in
        match to_unescape with
        | [] -> src
        | idx::to_unescape' ->
          let dst = create (String.length src - List.length to_unescape) in
          let rec loop last_idx last_dst_pos = function
            | [] ->
              (* copy "000" at last *)
              blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:last_idx
            | idx::to_unescape -> (* [idx] = index of escaping char *)
              (* take 1st iteration as example, calculate the length of "333", minus 2 to
                 skip '_C' *)
              let len = last_idx - idx - 2 in
              (* point [dst_pos] to the position to copy "333" to *)
              let dst_pos = last_dst_pos - len in
              (* copy "333" *)
              blit ~src ~src_pos:(idx + 2) ~dst ~dst_pos ~len;
              (* backoff [dst_pos] by 1 to copy 'c' *)
              let dst_pos = dst_pos - 1 in
              dst.[dst_pos] <-
                ( match escapeworthy.(Char.to_int src.[idx + 1]) with
                | -1 -> src.[idx + 1]
                | n -> Char.unsafe_of_int n);
              (* update [last_dst_pos] and [last_idx] *)
              loop idx dst_pos to_unescape
          in
          ( if idx < String.length src - 1 then
              (* set [last_dst_pos] and [last_idx] to length of [dst] and [src] *)
              loop (String.length src) (String.length dst) to_unescape
            else
              (* for escaped string ending with an escaping char like "000_", just ignore
                 the last escaping char *)
              loop (String.length src - 1) (String.length dst) to_unescape'
          );
          dst
      )
  ;;

  let unescape_gen_exn ~escapeworthy_map ~escape_char =
    Or_error.ok_exn (unescape_gen ~escapeworthy_map ~escape_char) |! stage
  ;;

  TEST_MODULE "unescape_gen" = struct
    let unescape =
      unstage
        (unescape_gen_exn ~escapeworthy_map:['%','p';'^','c'] ~escape_char:'_')

    TEST = unescape "__" = "_"
    TEST = unescape "foo" = "foo"
    TEST = unescape "__" = "_"
    TEST = unescape "foo_pbar" = "foo%bar"
    TEST = unescape "_cfoo_p" = "^foo%"

    let unescape2 =
      unstage
        (unescape_gen_exn ~escapeworthy_map:['_','.';'%','p';'^','c'] ~escape_char:'_')

    (* this one is ill-formed, just ignore the escape_char without escaped char *)
    TEST = unescape2 "_" = ""
    TEST = unescape2 "a_" = "a"

    TEST = unescape2 "__" = "_"
    TEST = unescape2 "_.." = "_."
    TEST = unescape2 "_." = "_"
    TEST = unescape2 "foo_p_.bar" = "foo%_bar"
    TEST = unescape2 "_.foo_p" = "_foo%"

    (* generate [n] random string and check if escaping and unescaping are consistent *)
    let random_test ~escapeworthy_map ~escape_char n =
      let escape =
        unstage (escape_gen_exn ~escapeworthy_map ~escape_char)
      in
      let unescape =
        unstage (unescape_gen_exn ~escapeworthy_map ~escape_char)
      in
      let test str =
        let escaped = escape str in
        let unescaped = unescape escaped in
        if str <> unescaped then
          failwith (
            Printf.sprintf
              "string: %s\nescaped string: %s\nunescaped string: %s"
              str escaped unescaped)
      in
      let array_random_elem arr =
        arr.(Random.int (Array.length arr))
      in
      let random_char =
        let print_chars =
          List.range (Char.to_int Char.min_value) (Char.to_int Char.max_value + 1)
          |! List.filter_map ~f:Char.of_int
          |! List.filter ~f:Char.is_print
          |! Array.of_list
        in
        fun () -> array_random_elem print_chars
      in
      let escapeworthy_chars =
        List.map escapeworthy_map ~f:fst |! Array.of_list
      in
      try
        for _i = 0 to n - 1 do
          let str =
            List.init (Random.int 50) ~f:(fun _ ->
              let p = Random.int 100 in
              if p < 10 then
                escape_char
              else if p < 25 then
                array_random_elem escapeworthy_chars
              else
                random_char ()
            )
            |! of_char_list
          in
          test str
        done;
        true
      with e ->
        raise e

    TEST = random_test 1000 ~escapeworthy_map:['%','p';'^','c'] ~escape_char:'_'
    TEST = random_test 1000 ~escapeworthy_map:['_','.';'%','p';'^','c'] ~escape_char:'_'
  end

  let unescape ~escape_char =
    unescape_gen_exn ~escapeworthy_map:[] ~escape_char

  TEST_MODULE "unescape" = struct
    let unescape = unstage (unescape ~escape_char:'_')
    TEST = unescape "foo" = "foo"
    TEST = unescape "__" = "_"
    TEST = unescape "foo_%bar" = "foo%bar"
    TEST = unescape "_^foo_%" = "^foo%"
  end

  let preceding_escape_chars str ~escape_char pos =
    let rec loop p cnt =
      if (p < 0) || (str.[p] <> escape_char) then
        cnt
      else
        loop (p - 1) (cnt + 1)
    in
    loop (pos - 1) 0
  ;;

  (* In an escaped string, any char is either `Escaping, `Escaped or `Literal. For
     example, the escape statuses of chars in string "a_a__" with escape_char = '_' are

       a : `Literal
       _ : `Escaping
       a : `Escaped
       _ : `Escaping
       _ : `Escaped

     [update_escape_status str ~escape_char i previous_status] gets escape status of
     str.[i] basing on escape status of str.[i - 1]
  *)
  let update_escape_status str ~escape_char i = function
    | `Escaping -> `Escaped
    | `Literal
    | `Escaped -> if str.[i] = escape_char then `Escaping else `Literal
  ;;

  let escape_status str ~escape_char pos =
    let odd = (preceding_escape_chars str ~escape_char pos) mod 2 = 1 in
    match odd, str.[pos] = escape_char with
    | true, (true|false) -> `Escaped
    | false, true -> `Escaping
    | false, false -> `Literal
  ;;

  let check_bound str pos function_name =
    if pos >= String.length str || pos < 0 then
      invalid_argf "%s: out of bounds" function_name ()
  ;;

  let is_char_escaping str ~escape_char pos =
    check_bound str pos "is_char_escaping";
    escape_status str ~escape_char pos = `Escaping
  ;;

  TEST_MODULE "is_char_escaping" = struct
    let is = is_char_escaping ~escape_char:'_'
    TEST = is "___" 0 = true
    TEST = is "___" 1 = false
    TEST = is "___" 2 = true (* considered escaping, though there's nothing to escape *)

    TEST = is "a_b__c" 0 = false
    TEST = is "a_b__c" 1 = true
    TEST = is "a_b__c" 2 = false
    TEST = is "a_b__c" 3 = true
    TEST = is "a_b__c" 4 = false
    TEST = is "a_b__c" 5 = false
  end

  let is_char_escaped str ~escape_char pos =
    check_bound str pos "is_char_escaped";
    escape_status str ~escape_char pos = `Escaped
  ;;

  TEST_MODULE "is_char_escaped" = struct
    let is = is_char_escaped ~escape_char:'_'
    TEST = is "___" 2 = false
    TEST = is "x" 0 = false
    TEST = is "_x" 1 = true
    TEST = is "sadflkas____sfff" 12 = false
    TEST = is "s_____s" 6 = true
  end

  let is_char_literal str ~escape_char pos =
    check_bound str pos "is_char_literal";
    escape_status str ~escape_char pos = `Literal
  ;;

  TEST_MODULE "is_char_literal" = struct
    let is_char_literal = is_char_literal ~escape_char:'_'
    TEST = is_char_literal "123456" 4 = true
    TEST = is_char_literal "12345_6" 6 = false
    TEST = is_char_literal "12345_6" 5 = false
    TEST = is_char_literal "123__456" 4 = false
    TEST = is_char_literal "123456__" 7 = false
    TEST = is_char_literal "__123456" 1 = false
    TEST = is_char_literal "__123456" 0 = false
    TEST = is_char_literal "__123456" 2 = true
  end

  let index_from str ~escape_char pos char =
    check_bound str pos "index_from";
    let rec loop i status =
      if i >= pos && status = `Literal && str.[i] = char then Some i
      else (
        let i = i + 1 in
        if i >= length str then None
        else loop i (update_escape_status str ~escape_char i status))
    in
    loop pos (escape_status str ~escape_char pos)
  ;;

  let index_from_exn str ~escape_char pos char =
    match index_from str ~escape_char pos char with
    | None ->
      failwiths "index_from_exn: not found"
        (str, `escape_char escape_char, `pos pos, char)
        <:sexp_of<t * [`escape_char of char] * [`pos of int] * char>>
    | Some pos -> pos
  ;;

  let index str ~escape_char char = index_from str ~escape_char 0 char
  let index_exn str ~escape_char char = index_from_exn str ~escape_char 0 char

  TEST_MODULE "index_from" = struct
    let f = index_from ~escape_char:'_'
    TEST = f "__" 0 '_' = None
    TEST = f "_.." 0 '.' = Some 2
    TEST = f "1273456_7789" 3 '7' = Some 9
    TEST = f "1273_7456_7789" 3 '7' = Some 11
    TEST = f "1273_7456_7789" 3 'z' = None
  end

  let rindex_from str ~escape_char pos char =
    check_bound str pos "rindex_from";
    (* if the target char is the same as [escape_char], we have no way to determine which
       escape_char is literal, so just return None *)
    if char = escape_char then None
    else
      let rec loop pos =
        if pos < 0 then None
        else (
          let escape_chars = preceding_escape_chars str ~escape_char pos in
          if escape_chars mod 2 = 0 && str.[pos] = char
          then Some pos else loop (pos - escape_chars - 1))
      in
      loop pos
  ;;

  let rindex_from_exn str ~escape_char pos char =
    match rindex_from str ~escape_char pos char with
    | None ->
      failwiths "rindex_from_exn: not found"
        (str, `escape_char escape_char, `pos pos, char)
        <:sexp_of<t * [`escape_char of char] * [`pos of int] * char>>
    | Some pos -> pos
  ;;

  let rindex str ~escape_char char =
    rindex_from str ~escape_char (String.length str - 1) char
  ;;

  let rindex_exn str ~escape_char char =
    rindex_from_exn str ~escape_char (String.length str - 1) char
  ;;

  TEST_MODULE "rindex_from" = struct
    let f = rindex_from ~escape_char:'_'
    TEST = f "__" 0 '_' = None
    TEST = f "123456_37839" 9 '3' = Some 2
    TEST = f "123_2321" 6 '2' = Some 6
    TEST = f "123_2321" 5 '2' = Some 1
  end

  (* [split_gen str ~escape_char ~on] works similarly to [String.split_gen], with an
     additional requirement: only split on literal chars, not escaping or escaped *)
  let split_gen str ~escape_char ~on =
    let is_delim = match on with
      | `char c' -> (fun c -> c = c')
      | `char_list l -> (fun c -> char_list_mem l c)
    in
    let len = String.length str in
    let rec loop acc status last_pos pos =
      if pos = len then
        List.rev (String.sub str ~pos:last_pos ~len:(len - last_pos) :: acc)
      else
        let status = update_escape_status str ~escape_char pos status in
        if status = `Literal && is_delim str.[pos] then
          let sub_str = String.sub str ~pos:last_pos ~len:(pos - last_pos) in
          loop (sub_str :: acc) status (pos + 1) (pos + 1)
        else loop acc status last_pos (pos + 1)
    in
    loop [] `Literal 0 0
  ;;

  let split str ~on = split_gen str ~on:(`char on) ;;

  let split_on_chars str ~on:chars =
    split_gen str ~on:(`char_list chars)
  ;;

  TEST_MODULE "split_on_gen" = struct
    let split = split_gen ~escape_char:'_' ~on:(`char ',')
    TEST = split "foo,bar,baz" = ["foo"; "bar"; "baz"]
    TEST = split "foo_,bar,baz" = ["foo_,bar"; "baz"]
    TEST = split "foo_,bar_,baz" = ["foo_,bar_,baz"]
    TEST = split "foo__,bar,baz" = ["foo__"; "bar"; "baz"]
    TEST = split "foo,bar,baz_," = ["foo"; "bar"; "baz_,"]
    TEST = split "foo,bar_,baz_,," = ["foo"; "bar_,baz_,"; ""]

    let split = split_gen ~escape_char:'_' ~on:(`char_list [',';':'])
    TEST = split "foo,bar:baz" = ["foo"; "bar"; "baz"]
    TEST = split "foo_,bar,baz" = ["foo_,bar"; "baz"]
    TEST = split "foo_:bar_,baz" = ["foo_:bar_,baz"]
    TEST = split "foo,bar,baz_," = ["foo"; "bar"; "baz_,"]
    TEST = split "foo:bar_,baz_,," = ["foo"; "bar_,baz_,"; ""]
  end

  let split_at str pos =
      String.sub str ~pos:0 ~len:pos,
      String.sub str ~pos:(pos + 1) ~len:(String.length str - pos - 1)
  ;;

  let lsplit2 str ~on ~escape_char =
    Option.map (index str ~escape_char on) ~f:(fun x -> split_at str x)
  ;;

  let rsplit2 str ~on ~escape_char =
    Option.map (rindex str ~escape_char on) ~f:(fun x -> split_at str x)
  ;;

  let lsplit2_exn str ~on ~escape_char =
    split_at str (index_exn str ~escape_char on)
  ;;
  let rsplit2_exn str ~on ~escape_char =
    split_at str (rindex_exn str ~escape_char on)
  ;;

  TEST_MODULE "split2" = struct
    let escape_char = '_'
    let on = ','
    TEST = lsplit2 ~escape_char ~on "foo_,bar,baz_,0" = Some ("foo_,bar", "baz_,0")
    TEST = rsplit2 ~escape_char ~on "foo_,bar,baz_,0" = Some ("foo_,bar", "baz_,0")
    TEST = lsplit2_exn ~escape_char ~on "foo_,bar,baz_,0" = ("foo_,bar", "baz_,0")
    TEST = rsplit2_exn ~escape_char ~on "foo_,bar,baz_,0" = ("foo_,bar", "baz_,0")
    TEST = lsplit2 ~escape_char ~on "foo_,bar" = None
    TEST = rsplit2 ~escape_char ~on "foo_,bar" = None
    TEST = try lsplit2_exn ~escape_char ~on "foo_,bar" |! Fn.const false with _ -> true
    TEST = try rsplit2_exn ~escape_char ~on "foo_,bar" |! Fn.const false with _ -> true
  end
end
;;

module Replace_polymorphic_compare = struct
  let equal = equal
  let min (x : t) y = if x < y then x else y
  let max (x : t) y = if x > y then x else y
  let compare (x : t) y = compare x y
  let ascending = compare
  let descending x y = compare y x
  let ( >= ) x y = (x : t) >= y
  let ( <= ) x y = (x : t) <= y
  let ( = ) x y = (x : t) = y
  let ( > ) x y = (x : t) > y
  let ( < ) x y = (x : t) < y
  let ( <> ) x y = (x : t) <> y
  let between t ~low ~high = low <= t && t <= high
  let _squelch_unused_module_warning_ = ()
end

include Replace_polymorphic_compare
