open! Import
include Base.Exn

let install_sexp_converters () =
  StdLabels.List.iter
    ~f:(fun (extension_constructor, handler) ->
      Sexplib.Conv.Exn_converter.add ~finalise:false extension_constructor handler)
    [
      (
        [%extension_constructor Bin_prot.Common.Read_error],
        (function
        | Bin_prot.Common.Read_error (err, pos) ->
            let str_err = Bin_prot.Common.ReadError.to_string err in
            Sexp.List [
              Sexp.Atom "Bin_prot.Common.Read_error";
              Sexp.Atom str_err;
              [%sexp (pos : int)];
            ]
        | _ -> assert false)
      );
    ]

let initialize_module () =
  initialize_module ();
  install_sexp_converters ();
;;
