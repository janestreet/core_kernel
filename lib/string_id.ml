open Std_internal

module type S = sig
  include Identifiable with type t = private string

  module Stable : sig
    module V1 : sig
      type nonrec t = t with sexp, bin_io, compare
    end
  end
end

module Make (M : sig val module_name : string end) = struct
  module Stable = struct
    module V1 = struct
      include String

      let check s =
        let invalid reason =
          Error (sprintf "'%s' is not a valid %s because %s" s M.module_name reason)
        in
        let stripped = String.strip s in
        if not (String.(=) stripped s)
        then invalid "it has whitespace on the edge"
        else if String.(=) s ""
        then invalid "it is empty"
        else if String.contains s '|'
        then invalid "it contains a pipe '|'"
        else Ok ()
      ;;

      let of_string s =
        match check s with
        | Ok () -> s
        | Error err -> invalid_arg err

      let t_of_sexp sexp =
        let s = String.t_of_sexp sexp in
        match check s with
        | Ok () -> s
        | Error err -> of_sexp_error err sexp
    end
  end

  include Stable.V1

end

include Make (struct let module_name = "String_id" end)
