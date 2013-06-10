(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
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
          A "caml_modify"])
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
