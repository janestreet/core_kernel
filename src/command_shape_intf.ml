(** A [Command_shape] allows limited introspection of a [Command], including subcommands,
    arguments, and doc strings. Think of it as machine-readable help. *)

open! Import
open! Std_internal

module type Command_shape = sig
  module Anons : sig
    module Grammar : sig
      type t =
        | Zero
        | One of string
        | Many of t
        | Maybe of t
        | Concat of t list
        | Ad_hoc of string
      [@@deriving bin_shape, compare, sexp_of]

      include Invariant.S with type t := t

      val usage : t -> string
    end

    type t =
      | Usage of string
      (** When exec'ing an older binary whose help sexp doesn't expose the grammar. *)
      | Grammar of Grammar.t
    [@@deriving bin_shape, compare, sexp_of]
  end

  module Flag_info : sig
    type t =
      { name : string
      ; doc : string
      ; aliases : string list
      }
    [@@deriving compare, fields, sexp_of]

    include
      Binable.S with type t := t
      [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Flag_info]."]

    val t_of_sexp : Sexp.t -> t
    [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Flag_info]."]

    val sort : t list -> t list
    val to_string : t list -> string
  end

  module Base_info : sig
    type t =
      { summary : string
      ; readme : string option
      ; anons : Anons.t
      ; flags : Flag_info.t list
      }
    [@@deriving compare, fields, sexp_of]

    val get_usage : t -> string

    include
      Binable.S with type t := t
      [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Base_info]."]

    val t_of_sexp : Sexp.t -> t
    [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Base_info]."]
  end

  module Group_info : sig
    type 'a t =
      { summary : string
      ; readme : string option
      ; subcommands : (string, 'a) List.Assoc.t Lazy.t
      }
    [@@deriving compare, fields, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t

    include
      Binable.S1 with type 'a t := 'a t
      [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Group_info]."]

    val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
    [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Group_info]."]
  end

  module Exec_info : sig
    type t =
      { summary : string
      ; readme : string option
      ; working_dir : string
      ; path_to_exe : string
      ; child_subcommand : string list
      }
    [@@deriving compare, fields, sexp_of]

    include
      Binable.S with type t := t
      [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Exec_info]."]

    val t_of_sexp : Sexp.t -> t
    [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Exec_info]."]
  end

  (** Fully forced shapes are comparable and serializable. *)
  module Fully_forced : sig
    type t =
      | Basic of Base_info.t
      | Group of t Group_info.t
      | Exec of Exec_info.t * t
    [@@deriving compare, sexp_of]

    include
      Binable.S with type t := t
      [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Fully_forced]."]

    val t_of_sexp : Sexp.t -> t
    [@@deprecated "[since 2020-04] Use [Command.Stable.Shape.Fully_forced]."]
  end

  type t =
    | Basic of Base_info.t
    | Group of t Group_info.t
    | Exec of Exec_info.t * (unit -> t)
    | Lazy of t Lazy.t

  val fully_forced : t -> Fully_forced.t
  val get_summary : t -> string

  module Sexpable : sig
    type t =
      | Base of Base_info.t
      | Group of t Group_info.t
      | Exec of Exec_info.t
      | Lazy of t Lazy.t
    [@@deriving sexp_of]

    val extraction_var : string
    val supported_versions : Set.M(Int).t

    module Versioned : sig
      type t [@@deriving sexp]
    end

    val of_versioned : Versioned.t -> t
    val to_versioned : t -> version_to_use:int -> Versioned.t
  end

  module Stable : sig
    module Anons : sig
      module Grammar : sig
        module V1 : Stable_without_comparator with type t = Anons.Grammar.t
      end

      module V2 : Stable_without_comparator with type t = Anons.t
    end

    module Flag_info : sig
      module V1 : Stable_without_comparator with type t = Flag_info.t
    end

    module Base_info : sig
      module V2 : Stable_without_comparator with type t = Base_info.t
    end

    module Group_info : sig
      module V2 : Stable1 with type 'a t = 'a Group_info.t
    end

    module Exec_info : sig
      module V3 : Stable_without_comparator with type t = Exec_info.t
    end

    module Fully_forced : sig
      module V1 : Stable_without_comparator with type t = Fully_forced.t
    end
  end

  module Private : sig
    val abs_path : dir:string -> string -> string
    val help_screen_compare : string -> string -> int
    val word_wrap : string -> int -> string list
  end
end
