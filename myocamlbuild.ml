(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let setup_preprocessor_deps = function
  | After_rules ->
    dep ["pp_deps_for_src"] ["src/config.h"];
  | _ -> ()

let dispatch = function
  | After_rules ->
    flag ["ocaml"; "link"; "native"; "caml_modify_wrapper"]
      (S [A "-cclib";
          A "-Xlinker";
          A "-cclib";
          A "--wrap";
          A "-cclib";
          A "-Xlinker";
          A "-cclib";
          A "caml_modify"]);

  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    setup_preprocessor_deps hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)
