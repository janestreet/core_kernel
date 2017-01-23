open! Import
open Std_internal
(* The code here refers to c functions that are not part of the library.  Instead, at link
   time of exe, bc, and toplevels, we include an object file that implements the given
   functions.  That way, we can update the functions to include the latest version info
   without having to recompile this library and everything that depends on it. *)
external generated_build_info : unit -> string = "generated_build_info"
external generated_hg_version : unit -> string = "generated_hg_version"

let trim_trailing_newline s =
  match (String.chop_suffix s ~suffix:"\n") with
  | Some s -> s
  | None -> s

let build_info = generated_build_info ()
let hg_version = trim_trailing_newline (generated_hg_version ())

let version = String.tr ~target:'\n' ~replacement:' ' hg_version
let version_list = String.split ~on:'\n' hg_version

let arg_spec = [
  ("-version",
   Arg.Unit
     (fun () ->
        print_endline hg_version;
        exit 0),
   " Print the hg revision of this build and exit");
  ("-build_info",
   Arg.Unit
     (fun () ->
        print_endline build_info;
        exit 0),
   " Print build info as sexp and exit");
]

module Application_specific_fields = struct
  type t = Sexp.t String.Map.t [@@deriving sexp]
end

type t = {
  username                    : string sexp_option;
  hostname                    : string sexp_option;
  kernel                      : string sexp_option;
  build_date                  : Date.t sexp_option;
  build_time                  : Time_float.Ofday.t sexp_option;
  x_library_inlining          : bool;
  dynlinkable_code            : bool;
  ocaml_version               : string;
  executable_path             : string;
  build_system                : string;
  application_specific_fields : Application_specific_fields.t sexp_option;
} [@@deriving of_sexp]

let build_info_as_sexp =
  Exn.handle_uncaught_and_exit (fun () -> Sexp.of_string build_info)
;;

let { username;
      hostname;
      kernel;
      build_date;
      build_time;
      x_library_inlining;
      dynlinkable_code;
      ocaml_version;
      executable_path;
      build_system;
      application_specific_fields;
    } =
  Exn.handle_uncaught_and_exit (fun () -> t_of_sexp build_info_as_sexp)
;;

let compiled_for_speed = x_library_inlining && not dynlinkable_code
