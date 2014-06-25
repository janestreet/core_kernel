(* This module is trying to minimize dependencies on modules in Core, so as to allow
   [Info], [Error], and [Or_error] to be used in is many places places as possible.
   Please avoid adding new dependencies. *)

open Sexplib.Std
open Bin_prot.Std

module Binable = Binable0

module Conv = Sexplib.Conv

module List = Core_list

module Sexp = struct
  include Sexplib.Sexp
  include (struct
    type t = Sexplib.Sexp.t = Atom of string | List of t list with bin_io
  end : Binable.S with type t := t)
end

type sexp = Sexp.t = Atom of string | List of sexp list (* constructor import *)

module Binable_exn = struct
  module T = struct
    type t = exn with sexp_of
  end
  include T
  include Bin_prot.Utils.Make_binable (struct
    module Binable = struct
      type t = Sexp.t with bin_io
    end
    include T
    exception Exn of Sexp.t with sexp
    let to_binable t = t |> <:sexp_of< t >>
    let of_binable sexp = Exn sexp
  end)
end

module Message = struct

  type t =
  | Could_not_construct of Sexp.t
  | String of string
  | Exn of Binable_exn.t
  | Sexp of Sexp.t
  | Tag_sexp of string * Sexp.t * Source_code_position0.t_hum option
  | Tag_t of string * t
  | Tag_arg of string * Sexp.t * t
  | Of_list of int option * t list
  | With_backtrace of t * string (* backtrace *)
  with bin_io, sexp_of

  let rec to_strings_hum t ac =
    (* We use [Sexp.to_string_mach], despite the fact that we are implementing
       [to_strings_hum], because we want the info to fit on a single line, and once
       we've had to resort to sexps, the message is going to start not looking so
       pretty anyway. *)
    match t with
    | Could_not_construct sexp ->
      "could not construct info: " :: Sexp.to_string_mach sexp :: ac
    | String string -> string :: ac
    | Exn exn -> Sexp.to_string_mach (Exn.sexp_of_t exn) :: ac
    | Sexp sexp -> Sexp.to_string_mach sexp :: ac
    | Tag_sexp (tag, sexp, _) -> tag :: ": " :: Sexp.to_string_mach sexp :: ac
    | Tag_t (tag, t) -> tag :: ": " :: to_strings_hum t ac
    | Tag_arg (tag, sexp, t) ->
      tag :: ": " :: Sexp.to_string_mach sexp :: ": " :: to_strings_hum t ac
    | With_backtrace (t, backtrace) ->
      to_strings_hum t ("\nBacktrace:\n" :: backtrace :: ac)
    | Of_list (trunc_after, ts) ->
      let ts =
        match trunc_after with
        | None -> ts
        | Some max ->
          let n = List.length ts in
          if n <= max then
            ts
          else
            List.take ts max @ [ String (Printf.sprintf "and %d more info" (n - max)) ]
      in
      List.fold (List.rev ts) ~init:ac ~f:(fun ac t ->
        to_strings_hum t (if List.is_empty ac then ac else ("; " :: ac)))
  ;;

  let to_string_hum_deprecated t = String.concat "" (to_strings_hum t [])

  let rec to_sexps_hum t ac =
    match t with
    | Could_not_construct _ as t -> sexp_of_t t :: ac
    | String string -> Atom string :: ac
    | Exn exn -> Exn.sexp_of_t exn :: ac
    | Sexp sexp -> sexp :: ac
    | Tag_sexp (tag, sexp, here) ->
      List ( Atom tag
             :: sexp
             :: (match here with
             | None -> []
             | Some here -> [ Source_code_position0.sexp_of_t_hum here ]))
      :: ac
    | Tag_t (tag, t) -> List (Atom tag :: to_sexps_hum t []) :: ac
    | Tag_arg (tag, sexp, t) -> List (Atom tag :: sexp :: to_sexps_hum t []) :: ac
    | With_backtrace (t, backtrace) ->
      Sexp.List [ to_sexp_hum t; Sexp.Atom backtrace ] :: ac
    | Of_list (_, ts) ->
      List.fold (List.rev ts) ~init:ac ~f:(fun ac t -> to_sexps_hum t ac)
  and to_sexp_hum t =
    match to_sexps_hum t [] with
    | [sexp] -> sexp
    | sexps -> Sexp.List sexps
  ;;
end

open Message

type t = Message.t Lazy.t

type t_ = t

(* We use [protect] to guard against exceptions raised by user-supplied functons, so
   that failure to produce one part of an info doesn't interfere with other parts. *)
let protect f = try f () with exn -> Message.Could_not_construct (Exn.sexp_of_t exn)

let to_message t = protect (fun () -> Lazy.force t)

let of_message message = Lazy.lazy_from_val message

let sexp_of_t t = Message.to_sexp_hum (to_message t)

let t_of_sexp sexp = lazy (Message.Sexp sexp)

let to_string_hum t =
  match to_message t with
  | String s -> s
  | message -> Sexp.to_string_hum (Message.to_sexp_hum message)
;;

let to_string_hum_deprecated t = Message.to_string_hum_deprecated (to_message t)

include Bin_prot.Utils.Make_binable (struct
  module Binable = Message
  type t = t_
  let to_binable = to_message
  let of_binable = of_message
end)

let of_lazy l = lazy (protect (fun () -> String (Lazy.force l)))

let of_string message = Lazy.lazy_from_val (String message)

TEST = to_string_hum (of_string "a\nb") = "a\nb"

let of_thunk f = lazy (protect (fun () -> String (f ())))

let create ?here tag x sexp_of_x =
  lazy (protect (fun () -> Tag_sexp (tag, sexp_of_x x, here)))
;;

let tag t tag = lazy (Tag_t (tag, to_message t))

let tag_arg t tag x sexp_of_x =
  lazy (protect (fun () -> Tag_arg (tag, sexp_of_x x, to_message t)))
;;

let of_list ?trunc_after ts =
  lazy (Of_list (?trunc_after, List.map ts ~f:to_message))
;;

exception Exn of t

let () =
  (* We install a custom exn-converter rather than use [exception Exn of t with sexp]
     to eliminate the extra wrapping of "(Exn ...)". *)
  Sexplib.Conv.Exn_converter.add_auto (Exn (of_string "<template>"))
    (function
    | Exn t -> sexp_of_t t
    | _ -> assert false)
;;

let to_exn t =
  if not (Lazy.is_val t)
  then Exn t
  else
    match Lazy.force t with
    | Message.Exn exn -> exn
    | _ -> Exn t
;;

let of_exn ?backtrace exn =
  let backtrace =
    match backtrace with
    | None -> None
    | Some `Get -> Some (Printexc.get_backtrace ())
    | Some (`This s) -> Some s
  in
  match exn, backtrace with
  | Exn t, None           -> t
  | Exn t, Some backtrace -> lazy (With_backtrace (to_message t, backtrace))
  | _    , None           -> Lazy.from_val (Message.Exn exn)
  | _    , Some backtrace -> lazy (With_backtrace (Sexp (Exn.sexp_of_t exn), backtrace))
;;

TEST_MODULE "Info" = struct

  let failwithf = Core_printf.failwithf

  let test_result got ~expect =
    if got <> expect then failwithf "(got %S) (expected %S)" got expect ();
  ;;

  TEST_UNIT =
    test_result (to_string_hum (of_exn (Failure "foo")))
      ~expect:"(Failure foo)"
  ;;

  TEST_UNIT =
    test_result (to_string_hum (tag (of_string "b") "a"))
      ~expect:"(a b)"
  ;;

  TEST_UNIT =
    test_result (to_string_hum (of_list (List.map ~f:of_string [ "a"; "b"; "c" ])))
      ~expect:"(a b c)"
  ;;

  TEST_UNIT =
    match to_exn (of_exn (Failure "foo")) with
    | Failure "foo" -> ()
    | exn -> failwithf "(got %S) (expected (Failure foo))" (Exn.to_string exn) ()
  ;;

  let round t =
    let sexp = sexp_of_t t in
    sexp = sexp_of_t (t_of_sexp sexp)
  ;;

  TEST = round (of_string "hello")
  TEST = round (of_thunk (fun () -> "hello"))
  TEST = round (create "tag" 13 <:sexp_of< int >>)
  TEST = round (tag (of_string "hello") "tag")
  TEST = round (tag_arg (of_string "hello") "tag" 13 <:sexp_of< int >>)
  TEST = round (of_list [ of_string "hello"; of_string "goodbye" ])
  TEST = round (t_of_sexp (Sexp.of_string "((random sexp 1)(b 2)((c (1 2 3))))"))
end

let pp ppf t = Format.pp_print_string ppf (to_string_hum t)
let () = Pretty_printer.register "Core.Info.pp"

(* yminsky: benchmarks

   open Core.Std
   module Bench = Core_extended.Bench

   let () =
   Bench.bench ~print:true (fun () ->
   let x = 33 in
   ignore (sprintf "%d" x)) ()
   |! Bench.print_costs

   let () =
   Bench.bench ~print:true (fun () ->
   let x = 33 in
   let closure = (fun () -> sprintf "%d" x) in
   ignore (if 3 = 4 then closure () else "")) ()
   |! Bench.print_costs

   Here are the bench results themselves:

   calculating cost of timing measurement: 260 ns
   calculating minimal measurable interval: 1000 ns
   determining number of runs per sample: 1048576
   stabilizing GC: done
   calculating the cost of a full major sweep: 1431398 ns
   running samples (estimated time 59 sec)
   ....................................................................................................
   mean run time + mean gc time: 568 ns
   warning: max run time is more than 5% away from mean
   calculating cost of timing measurement: 258 ns
   calculating minimal measurable interval: 1000 ns
   determining number of runs per sample: 134217728
   stabilizing GC: done
   calculating the cost of a full major sweep: 1484784 ns
   running samples (estimated time 75 sec)
   ....................................................................................................
   mean run time + mean gc time: 5 ns
   warning: max run time is more than 5% away from mean

*)
