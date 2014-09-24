open Core.Std
open Async.Std
open Ocaml_plugin.Std

module Compiler = Ocaml_compiler.Make(struct
  type t = (module Unit_intf.S)
  let t_repr = "Unit_intf.S";;
  let univ_constr = Unit_intf.univ_constr;;
  let univ_constr_repr = "Unit_intf.univ_constr";;
end)

let check_compilation s =
  let temp_file, temp_oc = Filename.open_temp_file "compilation_tests" ".ml" in
  Monitor.protect ~finally:(fun () -> Unix.unlink temp_file) (fun () ->
    let writer = Writer.of_out_channel temp_oc File in
    Writer.write writer ("open Core_kernel.Std\n" ^ s);
    Writer.close writer >>= fun () ->
    Compiler.check_ocaml_src_files [temp_file]
      (* same as default warnings, but don't warn about unused values *)
       ~custom_warnings_spec:"@a-4-29-40-41-42-44-45-34-32"
  )

module Test = struct
  (* The test is that [setup] compiles and [setup ^ shouldn't_compile] doesn't. *)
  type t = {
    note : string;
    setup : string;
    shouldn't_compile : string;
  } with sexp

  let test : t -> Error.t option Deferred.t = fun t ->
    check_compilation t.setup
    >>= function
    | Error e -> Deferred.return (
      Some (Error.tag_arg e "setup didn't compile" t <:sexp_of<t>>)
    )
    | Ok () ->
      check_compilation (t.setup ^ t.shouldn't_compile)
      >>| function
      | Error _ -> None
      | Ok () -> Some (Error.create "unexpectedly compiled" t <:sexp_of<t>>)
end

let tests : Test.t list = [
  { note = "Can't assign arbitrary types to ref"
  ; setup = "
type foo
let r = Ref.Permissioned.create 0"
  ; shouldn't_compile = "
let r' = (r : (int, foo) Ref.Permissioned.t)"
  };
  { note = "Can't set a ref if some version has been made immutable"
  ; setup = "
let r = Ref.Permissioned.create 0
let r_immutable = (r : (int, immutable_perms) Ref.Permissioned.t)
"
  ; shouldn't_compile = "
let () = Ref.Permissioned.set r 1
"
  };
  { note = "Can't make a ref immutable if it's been set"
  ; setup = "
let r = Ref.Permissioned.create 0
let () = Ref.Permissioned.set r 1
"
  ; shouldn't_compile = "
let r_immutable = (r : (int, immutable_perms) Ref.Permissioned.t)
"
  }
]


let () =
  (Deferred.List.filter_map tests ~f:Test.test
   >>> fun errors ->
   if List.is_empty errors
   then shutdown 0
   else begin
     List.iter errors ~f:(fun e -> eprintf "%s\n" (Error.to_string_hum e));
     shutdown 1
   end
  );
  never_returns (Scheduler.go ())
