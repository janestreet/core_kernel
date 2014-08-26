(* OASIS_START *)
(* OASIS_STOP *)

let dispatch env = function
  | After_rules ->
    pflag ["compile"; "ocaml"] "I" (fun x -> S [A "-I"; A x]);

    dep  ["ocaml"; "ocamldep"; "mlh"] ["lib/config.mlh"];

    flag ["mlh"; "ocaml"; "ocamldep"] (S[A"-ppopt"; A"-Ilib/"]);
    flag ["mlh"; "ocaml"; "compile"]  (S[A"-ppopt"; A"-Ilib/"]);
    flag ["mlh"; "ocaml"; "doc"]      (S[A"-ppopt"; A"-Ilib/"]);

    flag ["ocaml"; "link"; "native"; "caml_modify_wrapper"]
      (S [A "-cclib";
          A "-Xlinker";
          A "-cclib";
          A "--wrap";
          A "-cclib";
          A "-Xlinker";
          A "-cclib";
          A "caml_modify"]);

    flag ["ocaml"; "native"; "compile"; "inline0"] (S[A"-inline"; A"0"]);

    List.iter
      (fun tag ->
         pflag ["ocaml"; tag] "pa_ounit_lib"
           (fun s ->
              let args =
                if BaseEnvLight.var_get "tests" env = "true"
                then []
                else [A"-ppopt"; A"-pa-ounit-drop"]
              in
              let args = A"-ppopt" :: A"-pa-ounit-lib" :: A"-ppopt" :: A s :: args in
              S args))
      ["ocamldep"; "compile"; "doc"];
  | _ ->
    ()

let () =
  let env = BaseEnvLight.load ~allow_empty:true ~filename:MyOCamlbuildBase.env_filename () in
  Ocamlbuild_plugin.dispatch (fun hook -> dispatch env hook; dispatch_default hook)
