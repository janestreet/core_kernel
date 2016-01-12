open Std_internal

let debug = ref false

module Unpack_one = struct
  type ('value, 'partial_unpack) unpacked
    =  ?partial_unpack : 'partial_unpack
    -> ?pos            : int
    -> ?len            : int
    -> Bigstring.t
    -> [ `Ok of 'value * int
       | `Not_enough_data of 'partial_unpack * int
       | `Invalid_data of Error.t
       ]

  type 'a t = T : ('a, _) unpacked -> 'a t

  let create_unpacked unpack_one =
    fun ?partial_unpack ?pos ?len buf ->
      let (pos, len) =
        Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(Bigstring.length buf)
      in
      unpack_one ?partial_unpack buf ~pos ~len
  ;;

  let create unpack_one = T (create_unpacked unpack_one)

  include Monad.Make (struct

      type nonrec 'a t = 'a t

      let return v = T (fun ?partial_unpack:_ ?pos:_ ?len:_ _ -> `Ok (v, 0))

      let map' (t : 'a t) ~f =
        let T t = t in
        T (fun ?partial_unpack ?pos ?len buf ->
          match t ?partial_unpack ?pos ?len buf with
          | `Invalid_data _ | `Not_enough_data _ as x -> x
          | `Ok (a, pos) -> `Ok (f a, pos))
      ;;

      let map = `Custom map'

      let bind =
        let module Partial_unpack = struct
          type ('pa, 'b) t =
            | A : 'pa -> ('pa, _) t
            | B : 'pb * ('b, 'pb) unpacked -> (_, 'b) t
        end in
        let open Partial_unpack in
        let do_b ~na partial_unpack (ub : (_, _) unpacked) buf ~pos ~len =
          match ub ?partial_unpack ~pos ~len buf with
          | `Invalid_data _ as x -> x
          | `Not_enough_data (pb, nb) -> `Not_enough_data (B (pb, ub), nb + na)
          | `Ok (b, nb) -> `Ok (b, na + nb)
        in
        fun (T ua) f ->
          let do_a partial_unpack buf ~pos ~len =
            match ua ?partial_unpack ~pos ~len buf with
            | `Invalid_data _ as x -> x
            | `Not_enough_data (pa, n) -> `Not_enough_data (A pa, n)
            | `Ok (a, na) ->
              let T ub = f a in
              do_b ~na None ub buf ~pos:(pos + na) ~len:(len - na)
          in
          create (fun ?partial_unpack buf ~pos ~len ->
            match partial_unpack with
            | None              -> do_a       None         buf ~pos ~len
            | Some (A pa)       -> do_a       (Some pa)    buf ~pos ~len
            | Some (B (pb, ub)) -> do_b ~na:0 (Some pb) ub buf ~pos ~len)
      ;;
    end)

  (* [create_bin_prot] doesn't use [Bigstring.read_bin_prot] for performance reasons.  It
     was written prior to [Bigstring.read_bin_prot], and it's not clear whether switching
     to use it would cause too much of a performance hit. *)
  let create_bin_prot_unpacked bin_prot_reader =
    let header_length = Bin_prot.Utils.size_header_length in
    let not_enough_data = `Not_enough_data ((), 0) in
    let pos_ref = ref 0 in
    let invalid_data message a sexp_of_a =
      `Invalid_data (Error.create message a sexp_of_a)
    in
    let read bin_reader buf ~pos ~len =
      pos_ref := pos;
      let result = bin_reader buf ~pos_ref in
      if !pos_ref <> pos + len then
        invalid_data "pos_ref <> pos + len" (!pos_ref, pos, len)
          ([%sexp_of: int * int * int])
      else
        `Ok result
    in
    create_unpacked
      (fun ?partial_unpack:_ buf ~pos ~len ->
         if header_length > len then
           not_enough_data
         else begin
           match read Bin_prot.Utils.bin_read_size_header buf ~pos ~len:header_length with
           | `Invalid_data _ as x -> x
           | `Ok element_length ->
             if element_length < 0 then
               invalid_data "negative element length %d" element_length [%sexp_of: int]
             else begin
               if element_length > len - header_length then
                 not_enough_data
               else begin
                 match
                   read bin_prot_reader.Bin_prot.Type_class.read
                     buf ~pos:(pos + header_length) ~len:element_length
                 with
                 | `Invalid_data _ as x -> x
                 | `Ok result -> `Ok (result, header_length + element_length)
               end
             end
         end)
  ;;

  let create_bin_prot bin_prot_reader = T (create_bin_prot_unpacked bin_prot_reader)

  let sexp =
    let module Parse_pos = Sexp.Parse_pos in
    let partial_unpack_init ~pos ~len buf =
      Sexp.parse_bigstring buf ~len ~parse_pos:(Parse_pos.create ~buf_pos:pos ())
    in
    create
      (fun ?(partial_unpack = partial_unpack_init) buf ~pos ~len ->
         try
           begin match partial_unpack ~pos ~len buf with
           | Cont (_state, k)       -> `Not_enough_data (k, len)
           | Done (sexp, parse_pos) -> `Ok (sexp, parse_pos.Parse_pos.buf_pos - pos)
           end
         with exn -> `Invalid_data (Error.of_exn exn))
  ;;

  let char =
    create (fun ?partial_unpack:_ buf ~pos ~len ->
      if len < 1 then
        `Not_enough_data ((), 0)
      else
        `Ok (Bigstring.get buf pos, 1))
  ;;

  module type Equal = sig
    type t [@@deriving sexp_of]
    val equal : t -> t -> bool
  end

  let expect (type a) (T u) (module E : Equal with type t = a) expected =
    T (fun ?partial_unpack ?pos ?len buf ->
      match u ?partial_unpack ?pos ?len buf with
      | `Invalid_data _ | `Not_enough_data _ as x -> x
      | `Ok (parsed, n) ->
        if E.equal expected parsed then
          `Ok ((), n)
        else
          `Invalid_data
            (Error.create "parsed does not match expected" () (fun () ->
               [%sexp
                 { parsed =   (parsed   : E.t)
                 ; expected = (expected : E.t)
                 }])))
  ;;

  let expect_char = expect char (module Char)

  let newline = expect_char '\n'
end

type ('a, 'b) alive =
  { mutable partial_unpack : 'b option
  ; unpack_one             : ('a, 'b) Unpack_one.unpacked sexp_opaque
  (* [buf] holds unconsumed chars*)
  ; mutable buf            : Bigstring.t
  (* [pos] is the start of unconsumed data in[buf] *)
  ; mutable pos            : int
  (* [len] is the length of unconsumed data in[buf] *)
  ; mutable len            : int
  }
[@@deriving sexp_of]

type 'a state =
  | Alive  : ('a, _) alive -> 'a state
  | Dead  of Error.t
[@@deriving sexp_of]

type 'a t =
  { mutable state : 'a state
  }
[@@deriving sexp_of]

let invariant _ t =
  try
    match t.state with
    | Dead _ -> ()
    | Alive t ->
      assert (t.pos >= 0);
      assert (t.len >= 0);
      if t.len = 0 then assert (t.pos = 0);
      assert (t.pos + t.len <= Bigstring.length t.buf);
  with exn ->
    failwiths "invariant failed" (exn, t) [%sexp_of: exn * _ t]
;;

let create_unpacked ?partial_unpack unpack_one =
  { state =
      Alive { partial_unpack
            ; unpack_one
            ; buf = Bigstring.create 1
            ; pos = 0
            ; len = 0
            };
  }
;;

let create (Unpack_one.T unpack_one) = create_unpacked unpack_one

let create_bin_prot bin_prot_reader =
  create (Unpack_one.create_bin_prot bin_prot_reader)
;;

let is_empty t =
  match t.state with
  | Dead error -> Error error
  | Alive t -> Ok (is_none t.partial_unpack && t.len = 0)
;;

let is_empty_exn t = ok_exn (is_empty t)

let is_available t len =
  let input_start = t.pos + t.len in
  let available = Bigstring.length t.buf - input_start in
  available >= len
;;

let ensure_available t len =
  if not (is_available t len) then begin
    (* Grow the buffer, and shift the unconsumed bytes to the front. *)
    let new_buf = Bigstring.create (max (t.len + len) (2 * Bigstring.length t.buf)) in
    Bigstring.blito ~src:t.buf ~src_pos:t.pos ~src_len:t.len ~dst:new_buf ();
    t.pos <- 0;
    t.buf <- new_buf;
    assert (is_available t len);
  end;
;;

let feed_gen buf_length (blit_buf_to_bigstring : (_, _) Blit.blito)
      ?pos ?len t buf =
  if !debug then invariant ignore t;
  match t.state with
  | Dead e -> Error e
  | Alive t ->
    let (src_pos, src_len) =
      Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(buf_length buf)
    in
    ensure_available t src_len;
    blit_buf_to_bigstring
      ~src:buf ~src_pos ~src_len
      ~dst:t.buf ~dst_pos:(t.pos + t.len) ();
    t.len <- t.len + src_len;
    Ok ();
;;

let feed ?pos ?len t buf =
  feed_gen Bigstring.length Bigstring.blito             ?pos ?len t buf
;;

let feed_string ?pos ?len t buf =
  feed_gen    String.length Bigstring.From_string.blito ?pos ?len t buf
;;

let unpack_iter t ~f =
  if !debug then invariant ignore t;
  match t.state with
  | Dead e -> Error e
  | Alive alive ->
    let error e =
      t.state <- Dead e;
      Error e
    in
    let t = alive in
    let consume ~num_bytes =
      t.pos <- t.pos + num_bytes;
      t.len <- t.len - num_bytes;
    in
    let rec loop () =
      if t.len = 0 then begin
        t.pos <- 0;
        Ok ();
      end else begin
        match
          t.unpack_one t.buf ~pos:t.pos ~len:t.len ?partial_unpack:t.partial_unpack
        with
        | exception exn -> error (Error.create "unpack error" exn [%sexp_of: Exn.t])
        | unpack_result ->
          match unpack_result with
          | `Invalid_data e -> error (Error.tag e "invalid data")
          | `Ok (one, num_bytes) ->
            (* In order to get a value we either need to consume some bytes or have
               partially unpacked data, otherwise it is a bug in [unpack_one].  The case
               of [num_bytes = 0] comes up when parsing sexp atoms where we don't know
               where atom ends until we hit parenthesis, e.g. "abc(". *)
            if num_bytes < 0 || num_bytes > t.len then
              error (Error.create "unpack consumed invalid amount" num_bytes
                       [%sexp_of: int])
            else if num_bytes = 0 && Option.is_none t.partial_unpack then
              error (Error.of_string "\
unpack returned a value but consumed 0 bytes without partially unpacked data")
            else begin
              consume ~num_bytes;
              t.partial_unpack <- None;
              match f one with
              | exception exn ->
                error (Error.create "~f supplied to Unpack_buffer.unpack_iter raised" exn
                         [%sexp_of: exn])
              | _ -> loop ();
            end;
          | `Not_enough_data (partial_unpack, num_bytes) ->
            (* Partial unpacking need not have consumed any bytes, and cannot have
               consumed more bytes than were available. *)
            if num_bytes < 0 || num_bytes > t.len then
              error (Error.create "partial unpack consumed invalid amount" num_bytes
                       [%sexp_of: int])
            else begin
              consume ~num_bytes;
              t.partial_unpack <- Some partial_unpack;
              (* Put unconsumed bytes at the front.  We assume that unpacking is
                 deterministic, which ensures that every input byte is shifted at most
                 once.  Once a byte has been shifted, it will remain where it is until it
                 is consumed. *)
              if t.len > 0 then
                Bigstring.blito ~src:t.buf ~src_pos:t.pos ~src_len:t.len ~dst:t.buf ();
              t.pos <- 0;
              Ok ();
            end
      end
    in
    loop ()
;;

let unpack_into t q = unpack_iter t ~f:(Queue.enqueue q)

let%test_module "unpack-buffer" = (module struct

  let is_dead t =
    match t.state with
    | Dead _ -> true
    | Alive _ -> false
  ;;

  let unpack t =
    let q = Queue.create () in
    match unpack_into t q with
    | Ok () -> Ok q
    | Error _ as err ->
      (* If we *have* unpacked values, we first want to return them,
         and then on the next call we will return the error (because [t.state = Dead]) *)
      assert (is_dead t);
      if Queue.is_empty q then err else Ok q
  ;;

  module type Value = sig
    type t [@@deriving sexp_of]
    include Equal.S with type t := t

    val pack : t list -> string
    val unpack_one : t Unpack_one.t
  end

  let test (type value) (module V : Value with type t = value) values =
    let input = Bigstring.of_string (V.pack values) in
    let input_size = Bigstring.length input in
    for chunk_size = 1 to input_size do
      let t = create V.unpack_one in
      try
        assert (is_empty_exn t);
        let output = Queue.create () in
        let rec loop pos =
          if pos < input_size then begin
            let len = min chunk_size (input_size - pos) in
            assert (feed t input ~pos ~len = Ok ());
            assert (not (is_empty_exn t));
            let unpack_result = ok_exn (unpack t) in
            Queue.blit_transfer ~src:unpack_result ~dst:output ();
            loop (pos + len);
          end
        in
        loop 0;
        assert (is_empty_exn t);
        let output = Queue.to_list output in
        if not (List.equal ~equal:V.equal values output) then
          failwiths "mismatch" (values, output) [%sexp_of: V.t list * V.t list];
      with exn ->
        failwiths "failure"
          (exn, `chunk_size chunk_size, `input input, values, t)
          [%sexp_of: (exn
                       * [ `chunk_size of int ]
                       * [ `input of Bigstring.t ]
                       * V.t list
                       * V.t t)];
    done;
  ;;

  let%test_unit _ =
    debug := true;
    for value_size = 1 to 5 do
      let module Value = struct
        let pack ts = String.concat ts
        let unpack_one =
          Unpack_one.create
            (fun ?partial_unpack:_ buf ~pos ~len ->
               if len < value_size then
                 `Not_enough_data ((), 0)
               else
                 let string = String.create value_size in
                 Bigstring.To_string.blito ~src:buf ~src_pos:pos ~src_len:value_size
                   ~dst:string ();
                 `Ok (string, value_size))
        include String
      end in
      let values =
        List.init 10 ~f:(fun i ->
          String.init value_size ~f:(fun j ->
            Char.of_int_exn ((i * value_size + j) land 0xFF)))
      in
      test (module Value) values
    done
  ;;

  (* [Unpack_one.sexp] *)
  let%test_unit _ =
    let module Value = struct
      let pack ts = String.concat ~sep:" " (List.map ts ~f:Sexp.to_string)
      let unpack_one = Unpack_one.sexp
      include Sexp
    end in
    let sexps =
      Sexp.(
        let e = Atom "" in
        let a = Atom "a" in
        let abc = Atom "abc" in
        [ e; a; abc
        ; List []
        ; List [ a ]
        ; List [ e; a ]
        ; List [ List [] ]
        ; List [ List []
               ; List [ a ]
               ; List [ a; abc ]
               ]])
    in
    let test sexps = test (module Value) sexps in
    let terminator = Sexp.List [] in (* used to ensure unparsing succeeds *)
    List.iter sexps ~f:(fun sexp ->
      test [ sexp; terminator ];
      test [ sexp; sexp; terminator ]);
    test sexps
  ;;

  (* [Unpack_one.sexp] *)
  let%test_unit _ =
    debug := true;
    (* Error case. *)
    begin
      let Unpack_one.T unpack_sexp = Unpack_one.sexp in
      match unpack_sexp ~pos:0 ~len:1 (Bigstring.of_string ")") with
      | `Invalid_data _ -> ()
      | `Ok _
      | `Not_enough_data _ -> assert false
    end;
    (* Simple, case where we parse a complete sexp in one pass:
       - starts in the middle of the buffer
       - doesn't consume the whole buffer *)
    begin
      let Unpack_one.T unpack_sexp = Unpack_one.sexp in
      match unpack_sexp ~pos:1 ~len:9 (Bigstring.of_string ")(foo)(x y") with
      | `Ok (Sexp.List [Sexp.Atom "foo"], 5) -> ()
      | `Ok result ->
        Error.raise
          (Error.create "Unexpected result" result [%sexp_of: Sexp.t * int])
      | `Not_enough_data _
      | `Invalid_data _ -> assert false
    end;
    (* Partial sexp case, requries two passes to parse the sexp. *)
    begin
      let Unpack_one.T unpack_sexp = Unpack_one.sexp in
      match unpack_sexp ~pos:6 ~len:4 (Bigstring.of_string ")(foo)(x y") with
      | `Not_enough_data (k, 4) ->
        begin
          match
            unpack_sexp ~partial_unpack:k ~pos:0 ~len:3 (Bigstring.of_string " z)")
          with
          | `Ok (Sexp.List [Sexp.Atom "x"; Sexp.Atom "y"; Sexp.Atom "z"], 3) -> ()
          | `Ok _
          | `Not_enough_data _
          | `Invalid_data _ -> assert false
        end
      | `Not_enough_data (_, n) -> failwithf "Consumed %d bytes" n ()
      | `Ok result ->
        Error.raise
          (Error.create "Unexpected result" result [%sexp_of: Sexp.t * int])
      | `Invalid_data error -> Error.raise error
    end
  ;;

  (* [Unpack_one.ch] *)
  let%test_unit _ =
    debug := true;
    let succeeded_correctly = function
      | `Ok ((), 1) -> true
      | `Ok _ | `Not_enough_data _ | `Invalid_data _ -> false
    in
    let failed_correctly = function
      | `Invalid_data _ -> true
      | `Ok _ | `Not_enough_data _ -> false
    in
    let open Unpack_one in
    let T expect_a = expect_char 'a' in
    (* basic *)
    assert (succeeded_correctly (expect_a ~pos:0 ~len:1 (Bigstring.of_string "a")));
    assert (failed_correctly    (expect_a ~pos:0 ~len:1 (Bigstring.of_string "b")));
    (* middle of buffer *)
    assert (succeeded_correctly (expect_a ~pos:3 ~len:1 (Bigstring.of_string "bcda")));
    assert (failed_correctly    (expect_a ~pos:3 ~len:1 (Bigstring.of_string "abcd")));
    (* Need more data *)
    match expect_a ~pos:0 ~len:0 (Bigstring.of_string "") with
    | `Not_enough_data (_, 0) -> ()
    | `Not_enough_data _ | `Ok _ | `Invalid_data _ -> assert false
  ;;

  (* [Unpack_one.bind] *)
  let%test_unit _ =
    debug := true;
    let module Value = struct
      include Sexp
      let pack ts =
        List.map ts ~f:(fun sexp -> Sexp.to_string sexp ^ "\n")
        |> String.concat
      let unpack_one =
        let open Unpack_one.Monad_infix in
        Unpack_one.sexp
        >>= fun sexp ->
        Unpack_one.newline
        >>| fun () ->
        sexp
    end
    in
    test (module Value) [];
    test (module Value) [ List [] ];
    test (module Value) [ Atom "one" ];
    test (module Value) [ Atom "one"; Atom "two" ];
    test (module Value) [ Atom "one"; List [Atom "two"] ];
    test (module Value) [ List [Atom "one"] ; Atom "two" ]
  ;;

  (* [Unpack_one.create_bin_prot] *)
  let%test_unit _ =
    debug := true;
    let module Value = struct
      type t =
        { foo : bool
        ; bar : int
        ; baz : string list
        }
      [@@deriving bin_io, compare, sexp]

      let equal t t' = compare t t' = 0

      let pack ts =
        let size =
          List.fold ts ~init:0 ~f:(fun acc t ->
            acc + bin_size_t t + Bin_prot.Utils.size_header_length)
        in
        let buffer = Bigstring.create size in
        let final_pos =
          List.fold ts ~init:0 ~f:(fun pos t ->
            Bigstring.write_bin_prot buffer bin_writer_t t ~pos)
        in
        assert (final_pos = size);
        Bigstring.to_string buffer
      ;;

      let unpack_one = Unpack_one.create_bin_prot bin_reader_t
    end
    in
    let a = { Value. foo = true; bar = 4; baz = [ "qux"; "quux" ] } in
    let b = { Value. foo = false; bar = -3289; baz = [] } in
    let c = { Value. foo = false; bar = 0; baz = List.init 1000 ~f:(fun _i -> "spam") } in
    test (module Value) [];
    test (module Value) [ a ];
    test (module Value) [ a; b; a; a; a; b; b; c; c; b; a; a; b ];
    test (module Value) (List.init 1000 ~f:(fun i -> if i % 2 = 0 then a else b))
  ;;

  let%test_unit _ = (* [unpack_iter] when [f] raises *)
    let t =
      create (Unpack_one.create (fun ?partial_unpack:_ _ ~pos:_ ~len ->
        if len = 0
        then assert false
        else `Ok ((), 1)))
    in
    ok_exn (feed_string t "hello");
    assert (is_error (unpack_iter t ~f:(fun _ -> failwith "f raised")))
  ;;
end)
