(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

(* Temporary hacks *)
let js_hacks = function
  | After_rules ->
    rule "Generate a cmxs from a cmxa"
      ~dep:"%.cmxa"
      ~prod:"%.cmxs"
      ~insert:`top
      (fun env _ ->
         Cmd (S [ !Options.ocamlopt
                ; A "-shared"
                ; A "-linkall"
                ; A "-I"; A (Pathname.dirname (env "%"))
                ; A (env "%.cmxa")
                ; A "-o"
                ; A (env "%.cmxs")
            ]));

    (* Pass -predicates to ocamldep *)
    pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])
  | _ -> ()

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
  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    js_hacks hook;
    setup_preprocessor_deps hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)
