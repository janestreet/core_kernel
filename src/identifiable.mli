(** a signature for opaque identifier types. *)

module type S = sig
  type t with bin_io, sexp
  include Stringable.S         with type t := t
  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t
  include Pretty_printer.S     with type t := t
end

(** Used for making an Identifiable module.  Here's an example.

    {[
      module Id = struct
        module T = struct
          type t = A | B with bin_io, compare, sexp
          let hash (t : t) = Hashtbl.hash t
          include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
        end
        include T
        include Identifiable.Make (T)
      end
    ]}
*)
module Make (M : sig
  type t with bin_io, compare, sexp
  include Stringable.S with type t := t
  val hash : t -> int
  val module_name : string  (** for registering the pretty printer *)
end) : S with type t := M.t



