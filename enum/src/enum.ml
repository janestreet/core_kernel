open! Base
include Enum_intf

let command_friendly_name s =
  String.filter_map s ~f:(function
    | '\'' -> None
    | '_' -> Some '-'
    | c -> Some (Char.lowercase c))
;;

let atom_of_sexp_exn : Sexp.t -> string = function
  | Atom s -> s
  | List _ as sexp -> raise_s [%sexp "Enum.t expects atomic sexps.", (sexp : Sexp.t)]
;;

module Single = struct
  module type S = Sexp_of

  type 'a t = (module S with type t = 'a)

  let sexp_of (type a) ((module M) : a t) (a : a) = M.sexp_of_t a
  let atom_exn m a = atom_of_sexp_exn (sexp_of m a)
  let to_string_hum m a = command_friendly_name (atom_exn m a)

  let check_field_name t a field =
    [%test_eq: string] (to_string_hum t a) (command_friendly_name (Field.name field))
  ;;
end

type%template 'a t = ((module S with type t = 'a)[@mode p])
[@@mode p = (portable, nonportable)]

let to_string_hum (type a) ((module M) : a t) a = Single.to_string_hum (module M) a

let check_field_name (type a) ((module M) : a t) a field =
  Single.check_field_name (module M) a field
;;

let enum (type a) ((module M) : a t) =
  List.map M.all ~f:(fun a -> to_string_hum (module M) a, a)
;;

let assert_alphabetic_order_exn here (type a) ((module M) : a t) =
  let as_strings = List.map M.all ~f:(Single.atom_exn (module M)) in
  [%test_result: string list]
    ~here:[ here ]
    ~message:"This enumerable type is intended to be defined in alphabetic order"
    ~expect:(List.sort as_strings ~compare:String.compare)
    as_strings
;;

module%template.portable [@modality p] Rewrite_sexp_of (S : S [@modality p]) :
  S [@modality p] with type t = S.t = struct
  include S

  let sexp_of_t t = to_string_hum (module S) t |> Parsexp.Single.parse_string_exn
end

let%template[@mode (p, c) = ((nonportable, uncontended), (portable, contended))] arg_type
  (type a : value mod c p)
  ?case_sensitive
  ?key
  ?list_values_in_help
  (t : (a t[@mode p]) @ p)
  : _ @ p
  =
  let module S = (val t) in
  (Command.Arg_type.enumerated_sexpable [@mode p])
    ?key
    ?list_values_in_help
    ?case_sensitive
    (module Rewrite_sexp_of [@modality p] (S))
;;

module Make_param = struct
  type 'a t =
    { arg_type : 'a Command.Arg_type.t
    ; doc : string
    }

  let create ?case_sensitive ?key ?represent_choice_with ?list_values_in_help ~doc m =
    let doc =
      match represent_choice_with with
      | None -> " " ^ doc
      | Some represent_choice_with -> represent_choice_with ^ " " ^ doc
    in
    { arg_type = arg_type ?case_sensitive ?key ?list_values_in_help m; doc }
  ;;
end

type ('a, 'b) make_param =
  ?case_sensitive:bool
  -> ?represent_choice_with:string
  -> ?list_values_in_help:bool
  -> ?aliases:string list
  -> ?key:'a Univ_map.Multi.Key.t
  -> string
  -> doc:string
  -> 'a t
  -> 'b Command.Param.t

let make_param
  ~f
  ?case_sensitive
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  flag_name
  ~doc
  m
  =
  let { Make_param.arg_type; doc } =
    Make_param.create
      ?case_sensitive
      ?key
      ?represent_choice_with
      ?list_values_in_help
      ~doc
      m
  in
  Command.Param.flag ?aliases flag_name ~doc (f arg_type)
;;

let make_param_optional_with_default_doc_sexp
  (type a)
  ~default
  ?case_sensitive
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  flag_name
  ~doc
  (m : a t)
  =
  let { Make_param.arg_type; doc } =
    Make_param.create
      ?case_sensitive
      ?key
      ?represent_choice_with
      ?list_values_in_help
      ~doc
      m
  in
  Command.Param.flag_optional_with_default_doc_sexp
    ?aliases
    flag_name
    arg_type
    (fun default -> Sexp.Atom (to_string_hum m default))
    ~default
    ~doc
;;

let make_param_one_of_flags
  ?(if_nothing_chosen = Command.Param.If_nothing_chosen.Raise)
  ?aliases
  ~doc
  m
  =
  Command.Param.choose_one
    ~if_nothing_chosen
    (List.map (enum m) ~f:(fun (name, enum) ->
       let aliases = Option.map aliases ~f:(fun aliases -> aliases enum) in
       let doc = doc enum in
       Command.Param.flag ?aliases name (Command.Param.no_arg_some enum) ~doc))
;;

let make_param_optional_one_of_flags ?aliases ~doc m =
  Command.Param.choose_one
    ~if_nothing_chosen:Return_none
    (List.map (enum m) ~f:(fun (name, enum) ->
       let aliases = Option.map aliases ~f:(fun aliases -> aliases enum) in
       let doc = doc enum in
       Command.Param.flag ?aliases name (Command.Param.no_arg_some enum) ~doc))
;;

let comma_separated_extra_doc m =
  let options =
    enum m
    |> List.map ~f:fst
    |> List.sort ~compare:[%compare: string]
    |> String.concat ~sep:", "
  in
  [%string "(can be comma-separated values: %{options})"]
;;

let make_param_optional_comma_separated
  ?allow_empty
  ?strip_whitespace
  ?unique_values
  ?case_sensitive
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  flag_name
  ~doc
  m
  =
  make_param
    ?case_sensitive
    ?represent_choice_with
    ?list_values_in_help
    ?aliases
    ?key
    flag_name
    m
    ~f:
      (Fn.compose
         Command.Param.optional
         (Command.Arg_type.comma_separated ?allow_empty ?strip_whitespace ?unique_values))
    ~doc:[%string {|%{doc} %{comma_separated_extra_doc m}|}]
;;

let make_param_optional_comma_separated_with_default_doc
  ?allow_empty
  ?strip_whitespace
  ?unique_values
  (type a)
  ~default
  ?case_sensitive
  ?represent_choice_with
  ?list_values_in_help
  ?aliases
  ?key
  flag_name
  ~doc
  (m : a t)
  =
  let { Make_param.arg_type; doc } =
    Make_param.create
      ?case_sensitive
      ?represent_choice_with
      ?list_values_in_help
      ?key
      ~doc:[%string {|%{doc} %{comma_separated_extra_doc m}|}]
      m
  in
  Command.Param.flag_optional_with_default_doc_sexp
    ?aliases
    flag_name
    (Command.Arg_type.comma_separated
       ?allow_empty
       ?strip_whitespace
       ?unique_values
       arg_type)
    (fun default ->
      Sexp.Atom (List.map ~f:(to_string_hum m) default |> String.concat ~sep:","))
    ~default
    ~doc
;;

module%template.portable Make_to_string (M : Sexp_of) : sig
  val to_string : M.t -> string
end = struct
  let to_string t = command_friendly_name (atom_of_sexp_exn [%sexp (t : M.t)])
end

let known_values (type a) (module M : S_to_string with type t = a) =
  List.fold
    [%all: M.t]
    ~init:(Map.empty (module String))
    ~f:(fun map t -> Map.set map ~key:(M.to_string t) ~data:t)
;;

let of_string ~known_values s =
  match Map.find known_values s with
  | None ->
    let known_values = Map.keys known_values in
    raise_s [%message "Unknown value." s (known_values : string list)]
  | Some t -> t
;;

module Make_of_string (M : S_to_string) : sig
  val of_string : string -> M.t
end = struct
  let known_values = lazy (known_values (module M))
  let of_string = [%eta1 of_string ~known_values:(force known_values)]
end

module%template Make_of_string (M : S_to_string [@modality p]) : sig @@ p
  val of_string : string -> M.t
end = struct
  let known_values = known_values (module M)
  let of_string = [%eta1 of_string ~known_values]
end
[@@modality p = portable]

module%template.portable [@modality p] Make_stringable (M : S [@modality p]) :
  Stringable.S with type t := M.t = struct
  include Make_to_string [@modality p] (M)

  include Make_of_string [@modality p] (struct
      type t = M.t

      let all = M.all
      let to_string = to_string
    end)
end
