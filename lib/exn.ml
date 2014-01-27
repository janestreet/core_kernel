module Sexp = Sexplib.Sexp
module Conv = Sexplib.Conv
open Sexplib.Std

let sexp_of_exn = Conv.sexp_of_exn
let sexp_of_exn_opt = Conv.sexp_of_exn_opt

type t = exn with sexp_of

exception Finally of t * t with sexp
exception Reraised of string * t with sexp

let reraise exc str =
  raise (Reraised (str, exc))

let reraisef exc format =
  Printf.ksprintf (fun str () -> reraise exc str) format

let () =
  StdLabels.List.iter
    ~f:(fun (exc, handler) ->
      Conv.Exn_converter.add_auto ~finalise:false exc handler)
    [
      (
        Bin_prot.Common.Read_error (Bin_prot.Common.ReadError.Neg_int8, 0),
        (function
        | Bin_prot.Common.Read_error (err, pos) ->
            let str_err = Bin_prot.Common.ReadError.to_string err in
            Sexp.List [
              Sexp.Atom "Bin_prot.Common.Read_error";
              Sexp.Atom str_err;
              Conv.sexp_of_int pos;
            ]
        | _ -> assert false)
      );
    ]

let to_string exc = Sexp.to_string_hum ~indent:2 (sexp_of_exn exc)
let to_string_mach exc = Sexp.to_string_mach (sexp_of_exn exc)

let sexp_of_t = sexp_of_exn

let protectx ~f x ~(finally : _ -> unit) =
  let res =
    try f x
    with exn ->
      (try finally x with final_exn -> raise (Finally (exn, final_exn)));
      raise exn
  in
  finally x;
  res
;;

let protect ~f ~finally = protectx ~f () ~finally

include Pretty_printer.Register_pp (struct
  type t = exn
  let pp ppf t =
    match sexp_of_exn_opt t with
    | Some sexp -> Sexp.pp_hum ppf sexp
    | None -> Format.pp_print_string ppf (Printexc.to_string t)
  ;;
  let module_name = "Core.Std.Exn"
end)

let backtrace = Printexc.get_backtrace

let handle_uncaught_aux ~exit f =
  try f ()
  with exc ->
    let bt = backtrace () in
    Format.eprintf "@[<2>Uncaught exception:@\n@\n@[%a@]@]@\n@.%!" pp exc;
    if Printexc.backtrace_status () then prerr_string bt;
    exit 1

let handle_uncaught_and_exit f = handle_uncaught_aux f ~exit

let handle_uncaught ~exit:must_exit f =
  handle_uncaught_aux f ~exit:(if must_exit then exit else ignore)

let reraise_uncaught str func =
  try func () with
  | exn -> raise (Reraised (str, exn))

let () =
  Printexc.register_printer (fun exc ->
    match sexp_of_exn_opt exc with
    | None -> None
    | Some sexp ->
      Some (Sexp.to_string_hum ~indent:2 sexp))

external clear_backtrace : unit -> unit = "clear_caml_backtrace_pos" "noalloc"
let raise_without_backtrace e =
  clear_backtrace ();
  Raise_without_backtrace.Rwb_std.raise_without_backtrace e
;;

TEST_MODULE = struct
  exception Test_exception

  TEST_UNIT "clear_backtrace" =
    begin try raise Test_exception with _ -> () end;
    assert (backtrace () <> "");
    clear_backtrace ();
    assert (backtrace () = "");
  ;;

  let check_if_empty_backtrace raise_f =
    clear_backtrace ();
    (* The call to [raise] installs a new saved backtrace.  Then, the call to [raise_f],
       if it's [raise], should save a new, different backtrace, while if it's
       [raise_without_backtrace], should clear the backtrace and then not install a new
       one when raising. *)
    let old_backtrace = try raise   Not_found      with Not_found      -> backtrace () in
    assert (old_backtrace <> "");
    let new_backtrace = try raise_f Test_exception with Test_exception -> backtrace () in
    assert (new_backtrace <> old_backtrace);
    new_backtrace = ""
  ;;

  TEST = not (check_if_empty_backtrace raise)
  TEST = check_if_empty_backtrace raise_without_backtrace
end

BENCH_MODULE "raise" = struct

  exception Test_exception

  let nested_raise raise_f depth =
    let rec loop d =
      if d = 0
      then raise_f Test_exception
      else loop (d - 1) + 1
    in
    (fun () ->
       try
         ignore (loop depth : int)
       with
       | Test_exception -> ())
  ;;

  let depths = [ 0; 10; 100; 1000; 10_000 ]

  BENCH_INDEXED "raise" depth depths = nested_raise raise depth

  BENCH_INDEXED "raise_without_backtrace" depth depths =
    nested_raise raise_without_backtrace  depth
  ;;
end
