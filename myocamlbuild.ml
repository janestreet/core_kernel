(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let setup_preprocessor_deps = function
  | After_rules ->
    dep ["pp_deps_for_src"] ["src/config.h"; "src/config.mlh"];
  | _ -> ()

let dispatch = function
  | After_rules ->
    flag  ["c"; "compile"; "needs_headers"] & S[ A"-I"; A "include" ];

    flag ["ocaml"; "link"; "native"; "caml_modify_wrapper"]
      (S [A "-cclib";
          A "-Xlinker";
          A "-cclib";
          A "--wrap";
          A "-cclib";
          A "-Xlinker";
          A "-cclib";
          A "caml_modify"]);

    flag ["ocaml"; "dep"; "open_result_wrapper"] (S [A "-open"; A "Result_wrapper"]);
    flag ["ocaml"; "compile"; "open_result_wrapper"] (S [A "-open"; A "Result_wrapper"]);

    (* If we don't have the following, then we get:

           Error: Unbound type constructor Result.t
    *)
    dep ["open_result_wrapper"] ["src/result_wrapper.cmi"
                                ;"src/core_result.cmi"];

    (* If we don't have the following, then we get:

           Error: Forward reference to Core_result in file ...

       when building the pack.
    *)
    dep ["open_result_wrapper"; "byte"] ["src/core_result.cmo"];
    dep ["open_result_wrapper"; "native"] ["src/core_result.cmx"];

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
