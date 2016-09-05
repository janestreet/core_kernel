open Sexplib

module type S = sig
  type t [@@deriving sexp]
end

module type S1 = sig
  type 'a t [@@deriving sexp]
end

module type S2 = sig
  type ('a, 'b) t [@@deriving sexp]
end

module type S3 = sig
  type ('a, 'b, 'c) t [@@deriving sexp]
end

module Stable = struct

  module Of_sexpable = struct
    module V1
        (Sexpable : S)
        (M : sig
           type t
           val to_sexpable : t -> Sexpable.t
           val of_sexpable : Sexpable.t -> t
         end)
      : S with type t := M.t =
    struct
      let t_of_sexp sexp =
        let s = Sexpable.t_of_sexp sexp in
        (try M.of_sexpable s with exn -> Conv.of_sexp_error_exn exn sexp)

      let sexp_of_t t = Sexpable.sexp_of_t (M.to_sexpable t)
    end
  end

  module Of_sexpable1 = struct
    module V1
        (Sexpable : S1)
        (M : sig
           type 'a t
           val to_sexpable : 'a t -> 'a Sexpable.t
           val of_sexpable : 'a Sexpable.t -> 'a t
         end)
      : S1 with type 'a t := 'a M.t =
    struct
      let t_of_sexp a_of_sexp sexp =
        let s = Sexpable.t_of_sexp a_of_sexp sexp in
        (try M.of_sexpable s with exn -> Conv.of_sexp_error_exn exn sexp)

      let sexp_of_t sexp_of_a t = Sexpable.sexp_of_t sexp_of_a (M.to_sexpable t)
    end
  end

  module Of_sexpable2 = struct
    module V1
        (Sexpable : S2)
        (M : sig
           type ('a, 'b) t
           val to_sexpable : ('a, 'b) t -> ('a, 'b) Sexpable.t
           val of_sexpable : ('a, 'b) Sexpable.t -> ('a, 'b) t
         end)
      : S2 with type ('a, 'b) t := ('a, 'b) M.t =
    struct
      let t_of_sexp a_of_sexp b_of_sexp sexp =
        let s = Sexpable.t_of_sexp a_of_sexp b_of_sexp sexp in
        (try M.of_sexpable s with exn -> Conv.of_sexp_error_exn exn sexp)

      let sexp_of_t sexp_of_a sexp_of_b t =
        Sexpable.sexp_of_t sexp_of_a sexp_of_b (M.to_sexpable t)
    end
  end

  module Of_stringable = struct
    module V1 (M : Stringable.S) : S with type t := M.t = struct
      let t_of_sexp sexp =
        match sexp with
        | Sexp.Atom s ->
          (try M.of_string s with exn -> Conv.of_sexp_error_exn exn sexp)
        | Sexp.List _ ->
          Conv.of_sexp_error
            "Sexpable.Of_stringable.t_of_sexp expected an atom, but got a list" sexp

      let sexp_of_t t = Sexp.Atom (M.to_string t)
    end
  end

  module To_stringable = struct
    module V1 (M : S) : Stringable.S with type t := M.t = struct
      let of_string x = Conv.of_string__of__of_sexp M.t_of_sexp x
      let to_string x = Conv.string_of__of__sexp_of M.sexp_of_t x
    end
  end

end

module Of_sexpable   = Stable.Of_sexpable.V1
module Of_sexpable1  = Stable.Of_sexpable1.V1
module Of_sexpable2  = Stable.Of_sexpable2.V1
module Of_stringable = Stable.Of_stringable.V1
module To_stringable = Stable.To_stringable.V1
