module List = Core_list
module String = Core_string

type tree =
  | Base of string
  | Append of tree * tree

type t = { len : int; tree : tree }

let of_string s = { len = String.length s; tree = Base s }

let to_string { len; tree } =
  match tree with
  | Base s -> s
  | Append (s1, s2) ->
    let buf = String.create len in
    (* [todo] avoids stack overflow (some usage patterns result in highly unbalanced
       trees, so the naive recursive approach doesn't work) *)
    let rec go todo start = function
      | Base s ->
        String.blit ~src:s ~src_pos:0 ~dst:buf ~dst_pos:start ~len:(String.length s);
        let start = start + String.length s in
        (match todo with
          | [] -> assert (start = len)
          | x :: xs ->
            go xs start x)
      | Append (s1, s2) ->
        go (s2 :: todo) start s1
    in
    go [s2] 0 s1;
    buf
;;

let (^) a b = { len = a.len + b.len; tree = Append (a.tree, b.tree) }

let rec add_to_buffer_internal buffer todo = function
  | Append (s1, s2) -> add_to_buffer_internal buffer (s2::todo) s1
  | Base s ->
    Buffer.add_string buffer s;
    begin match todo with
    | [] -> ()
    | x :: xs -> add_to_buffer_internal buffer xs x
    end
;;

let add_to_buffer { len = _; tree } buffer = add_to_buffer_internal buffer [] tree

let%test_unit _ =
  let r = (of_string "abc" ^ of_string "def") ^ (of_string "ghi" ^ of_string "jkl") in
  let buffer = Buffer.create 12 in
  add_to_buffer r buffer;
  [%test_result: String.t] ~expect:"abcdefghijkl" (Buffer.to_bytes buffer)
;;

let%test "no stack overflow" = to_string (
  List.fold_left ~init:(of_string "") ~f:(^) (
    List.init 1000000 ~f:(fun _x -> of_string "x")))
        = String.make 1000000 'x'
;;

let%test _ = to_string (of_string "") = ""
let%test _ = to_string (of_string "x") = "x"
let%test _ = to_string (of_string "ab" ^ of_string "cd" ^ of_string "efg") = "abcdefg"
let%test _ =
  let rec go = function
    | 0 -> of_string "0"
    | n -> go (n - 1) ^ of_string (string_of_int n) ^ go (n - 1)
  in
  to_string (go 4) = "0102010301020104010201030102010"
;;

let%bench "small on the right" =
  let rec go acc = function
    | 0 -> acc
    | n -> go (acc ^ of_string "bla") (n - 1)
  in
  to_string (go (of_string "") 2048)
;;

let%bench "balanced" =
  let rec go = function
    | 0 -> of_string "bla"
    | n -> go (n - 1) ^ go (n - 1)
  in
  to_string (go 11)
;;
