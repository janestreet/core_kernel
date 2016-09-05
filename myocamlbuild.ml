(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let setup_preprocessor_deps = function
  | After_rules ->
    dep ["pp_deps_for_base_src"] ["base/src/config.h"];
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

    let hack = "ugly_hack_to_workaround_ocamlbuild_nightmare" in
    mark_tag_used hack;
    dep [hack] [hack];

    let add_exts fn exts =
      List.map (fun ext -> fn ^ ext)  exts
    in

    let mlpack = "base/src/base0.mlpack" in
    rule hack
      ~prod:hack
      ~deps:(mlpack :: add_exts "base/src/base0" [".cmx"; ".cmi"; ".cmo"])
      (fun _ _ ->
         let mods = string_list_of_file mlpack in
         let to_remove =
           StdLabels.List.map mods ~f:(fun fn ->
             add_exts ("base/src/" ^ String.uncapitalize_ascii fn)
               [ ".cmx"
               ; ".cmi"
               ; ".cmo"
               ; ".ml"
               ; ".mli"
               ; ".ml.depends"
               ; ".mli.depends"
               ; ".o"
               ])
           |> List.concat
         in
         Seq
           [ Seq (List.map rm_f to_remove)
           ; Echo ([], hack) ])

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
