open! Base

module type Sexp_of = sig
  type t [@@deriving sexp_of]
end

module type Single = sig
  (** These functions take single values of ['a] instead of enumerating all of them. *)

  type 'a t

  (** Map a constructor name to a command-line string: downcase the name and convert [_]
      to [-]. *)
  val to_string_hum : 'a t -> 'a -> string

  val check_field_name : 'a t -> 'a -> (_, _, _) Field.t_with_perm -> unit
end

module type%template S = Command.Enumerable_sexpable [@modality p]
[@@modality p = (nonportable, portable)]

module type%template S_to_string = Command.Enumerable_stringable [@modality p]
[@@modality p = (nonportable, portable)]

module type Enum = sig
  module type S = S

  type%template 'a t = ((module S with type t = 'a)[@mode p])
  [@@mode p = (portable, nonportable)]

  include Single with type 'a t := 'a t

  val enum : 'a t -> (string * 'a) list
  val assert_alphabetic_order_exn : Source_code_position.t -> 'a t -> unit

  type ('a, 'b) make_param =
    ?case_sensitive:bool
    -> ?represent_choice_with:string
         (** If [represent_choice_with] is not passed, the documentation will be:

             {v
          -flag (choice1|choice2|...)     [doc]
             v}

             If there are many choices, this can cause this and other flags to have the
             documentation aligned very far to the right. To avoid that, the
             [represent_choice_with] flag can be passed as a shorter reference to the
             possible choices. Example:

             {v
          -flag CHOICE     [doc], CHOICE can be (choice1|choice2|...)
             v}

             [Command] does a much better job of aligning this. *)
    -> ?list_values_in_help:bool
    -> ?aliases:string list
    -> ?key:'a Univ_map.Multi.Key.t
    -> string
    -> doc:string
    -> 'a t
    -> 'b Command.Param.t

  val make_param : f:('a Command.Arg_type.t -> 'b Command.Flag.t) -> ('a, 'b) make_param

  val make_param_one_of_flags
    :  ?if_nothing_chosen:('a, 'a) Command.Param.If_nothing_chosen.t (** Default: Raise *)
    -> ?aliases:('a -> string list)
    -> doc:('a -> string)
    -> 'a t
    -> 'a Command.Param.t

  val make_param_optional_one_of_flags
    :  ?aliases:('a -> string list)
    -> doc:('a -> string)
    -> 'a t
    -> 'a option Command.Param.t

  val make_param_optional_with_default_doc_sexp : default:'a -> ('a, 'a) make_param

  val make_param_optional_comma_separated
    :  ?allow_empty:bool
    -> ?strip_whitespace:bool
    -> ?unique_values:bool
    -> ('a, 'a list option) make_param

  val make_param_optional_comma_separated_with_default_doc
    :  ?allow_empty:bool
    -> ?strip_whitespace:bool
    -> ?unique_values:bool
    -> default:'a list
    -> ('a, 'a list) make_param

  val%template arg_type
    : ('a : value mod c p).
    ?case_sensitive:bool
    -> ?key:'a Univ_map.Multi.Key.t @ p
    -> ?list_values_in_help:bool
    -> ('a t[@mode p]) @ p
    -> 'a Command.Arg_type.t @ p
  [@@mode (p, c) = ((nonportable, uncontended), (portable, contended))]

  (** Transform a string to be accepted by [Command]. This is the transformation that is
      applied throughout this module.

      The transformations are:
      + Single quotes get removed (since it's annoying to have to quote them when running
        commands manually)
      + Underscores get turned into dashes (just to hopefully have a uniform convention
        between the two)
      + Other characters get lowercased

      Note that this is *not* actually a complete list of transformations needed to make
      an arbitrary string "command-friendly": for example, double quotes are left alone.
      This is because the expectation is that the string came from something like a
      [[@@deriving sexp]] on a variant type, and while single quotes can appear in ocaml
      variants, double quotes cannot. *)
  val command_friendly_name : string -> string

  (** Defines [to_string] and [of_string] functions for [M], based on [M.sexp_of_t] and
      [M.all]. The sexp representation of [M.t] must be a sexp atom. *)
  module%template.portable [@modality p] Make_stringable (M : S [@modality p]) : sig
    include Stringable.S with type t := M.t
  end

  (** Defines an [of_string] function for [M], using [M.all] and [M.to_string]. Does not
      require [M] to be sexpable. *)
  module%template.portable
    [@modality p] Make_of_string
      (M : S_to_string
    [@modality p]) : sig
    val of_string : String.t -> M.t
  end

  (** Defines [to_string] for [M], based on [M.sexp_of_t]. The sexp representation of
      [M.t] must be a sexp atom. *)
  module%template.portable Make_to_string (M : Sexp_of) : sig
    val to_string : M.t -> String.t
  end

  module Single : sig
    module type S = Sexp_of

    type 'a t = (module S with type t = 'a)

    include Single with type 'a t := 'a t
  end
end
