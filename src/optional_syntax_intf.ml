open! Import

module type S = sig
  type t
  type value

  module Optional_syntax : sig
    val is_none         : t -> bool
    val unchecked_value : t -> value
  end
end

module type S1 = sig
  type 'a t
  type 'a value

  module Optional_syntax : sig
    val is_none         : _  t -> bool
    val unchecked_value : 'a t -> 'a value
  end
end

module type S2 = sig
  type ('a, 'b) t
  type ('a, 'b) value

  module Optional_syntax : sig
    val is_none         : _ t -> bool
    val unchecked_value : ('a, 'b) t -> ('a, 'b) value
  end
end

module type Optional_syntax = sig
  (** Interfaces for use with [match%optional].  Idiomatic usage is to have a module [M]
      like:

      {[
        module M : sig
          type t

          module Optional_syntax : Optional_syntax.S
            with type t := t
            with type value := ...
        end = struct
          ...

          module Optional_syntax = struct
            module Optional_syntax = struct
              let is_none = is_node
              let unchecked_value = unchecked_value
            end
          end
        end
      ]}

      Then, uses look like:

      {[
        let open M.Optional_syntax in
        match%optional m with
        | None   -> ?
        | Some v -> ?
      ]}

      The reason for the double [module Optional_syntax] is so that [open M.Optional_syntax]
      puts in scope only [module Optional_syntax]; [match%optional] then expands to
      references to [Optional_syntax.is_none] and [Optional_syntax.unchecked_value].

      For more details on the syntax extension, see [ppx/ppx_optional/README.md].
  *)

  module type S  = S
  module type S1 = S1
  module type S2 = S2
end
