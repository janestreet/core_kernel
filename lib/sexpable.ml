
open Sexplib

module type S = sig
  type t with sexp
end

module type S1 = sig
  type 'a t with sexp
end

module type S2 = sig
  type ('a, 'b) t with sexp
end

module type S3 = sig
  type ('a, 'b, 'c) t with sexp
end

(* for when you want the sexp representation of one type to be the same as that for some
   other isomorphic type *)
module Of_sexpable
  (S : S)
  (M : sig
    type t
    val to_sexpable : t -> S.t
    val of_sexpable : S.t -> t
  end)
  : S with type t := M.t =
struct
  let t_of_sexp sexp =
    let s = S.t_of_sexp sexp in
    (try M.of_sexpable s with exn -> Conv.of_sexp_error_exn exn sexp)

  let sexp_of_t t = S.sexp_of_t (M.to_sexpable t)
end

module Of_stringable (M : Stringable.S) : S with type t := M.t =
struct
  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s ->
      (try M.of_string s with exn -> Conv.of_sexp_error_exn exn sexp)
    | Sexp.List _ ->
      Conv.of_sexp_error
        "Sexpable.Of_stringable.t_of_sexp expected an atom, but got a list" sexp

  let sexp_of_t t = Sexp.Atom (M.to_string t)
end

module To_stringable (M : S) : Stringable.S with type t := M.t =
struct
  let of_string x = Conv.of_string__of__of_sexp M.t_of_sexp x
  let to_string x = Conv.string_of__of__sexp_of M.sexp_of_t x
end
