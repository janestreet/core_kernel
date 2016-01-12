open Std_internal

module Sexp = Sexplib.Sexp

type t = Printexc.raw_backtrace

let get ?(at_most_num_frames = Int.max_value) () =
  Printexc.get_callstack at_most_num_frames
;;

let to_string = Printexc.raw_backtrace_to_string

let sexp_of_t t =
  Sexp.List
    (List.map (String.split (to_string t) ~on:'\n')
       ~f:(fun x -> Sexp.Atom x))
;;

let%test_unit _ =
  let t = get () in
  assert (String.length (to_string t) > 0)
;;

module Exn = struct
  let set_recording = Printexc.record_backtrace
  let am_recording  = Printexc.backtrace_status
  let most_recent   = Printexc.get_backtrace

  (* We turn on backtraces by default if OCAMLRUNPARAM isn't set. *)
  let maybe_set_recording () =
    match Sys.getenv "OCAMLRUNPARAM" with
    | exception _ -> set_recording true
    | _ -> ()  (* the caller set something, they are responsible *)
  ;;

  let with_recording b ~f =
    let saved = am_recording () in
    set_recording b;
    protect ~f ~finally:(fun () -> set_recording saved)
  ;;

  let%test _ = "" = with_recording false ~f:most_recent
end

let initialize_module () =
  Exn.maybe_set_recording ();
;;
