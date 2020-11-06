module Stable = struct
  module Shape = Command_shape.Stable
end

open! Import
open! Std_internal
include Command_intf
module Shape = Command_shape

(* in order to define expect tests, we want to raise rather than exit if the code is
   running in the test runner process *)
let raise_instead_of_exit =
  match Ppx_inline_test_lib.Runtime.testing with
  | `Testing `Am_test_runner -> true
  | `Testing `Am_child_of_test_runner | `Not_testing -> false
;;

exception Exit_called of { status : int } [@@deriving sexp_of]

(* [raise_instead_of_exit]-respecting wrappers for [exit] and functions that call it *)
include struct
  let exit status =
    if raise_instead_of_exit then raise (Exit_called { status }) else exit status
  ;;

  module Exn = struct
    let to_string = Exn.to_string

    let handle_uncaught_and_exit f =
      if raise_instead_of_exit
      then (
        try f () with
        | Exit_called { status = 0 } as exn -> print_s [%sexp (exn : exn)])
      else Exn.handle_uncaught_and_exit f
    ;;
  end
end

let unwords xs = String.concat ~sep:" " xs
let unparagraphs xs = String.concat ~sep:"\n\n" xs

exception Failed_to_parse_command_line of string

let die fmt =
  Printf.ksprintf (fun msg () -> raise (Failed_to_parse_command_line msg)) fmt
;;

let help_screen_compare = Shape.Private.help_screen_compare

(* universal maps are used to pass around values between different bits
   of command line parsing code without having a huge impact on the
   types involved

   1. passing values from parsed args to command-line autocomplete functions
   2. passing special values to a base commands that request them in their spec
 * expanded subcommand path
 * args passed to the base command
 * help text for the base command
*)
module Env = struct
  include Univ_map

  let key_create name = Univ_map.Key.create ~name sexp_of_opaque
  let multi_add = Univ_map.Multi.add
  let set_with_default = Univ_map.With_default.set
end

module Auto_complete = struct
  type t = Env.t -> part:string -> string list
end

module Completer = struct
  type t = Auto_complete.t option

  let run_and_exit t env ~part : never_returns =
    Option.iter t ~f:(fun completions ->
      List.iter ~f:print_endline (completions env ~part));
    exit 0
  ;;
end

module Arg_type = struct
  type 'a t =
    { parse : string -> ('a, exn) Result.t
    ; complete : Completer.t
    ; key : 'a Univ_map.Multi.Key.t option
    }

  let create ?complete ?key of_string =
    let parse x = Result.try_with (fun () -> of_string x) in
    { parse; key; complete }
  ;;

  let map ?key t ~f =
    let parse str = Result.map (t.parse str) ~f in
    let complete = t.complete in
    { parse; complete; key }
  ;;

  let string = create Fn.id
  let int = create Int.of_string
  let char = create Char.of_string
  let float = create Float.of_string
  let date = create Date.of_string
  let percent = create Percent.of_string
  let host_and_port = create Host_and_port.of_string
  let sexp = create Sexp.of_string
  let sexp_conv of_sexp = create (fun s -> of_sexp (Sexp.of_string s))

  let of_map ?key map =
    create
      ?key
      ~complete:(fun _ ~part:prefix ->
        List.filter_map (Map.to_alist map) ~f:(fun (name, _) ->
          if String.is_prefix name ~prefix then Some name else None))
      (fun arg ->
         match Map.find map arg with
         | Some v -> v
         | None ->
           failwithf "valid arguments: {%s}" (String.concat ~sep:"," (Map.keys map)) ())
  ;;

  let of_alist_exn ?key alist =
    match String.Map.of_alist alist with
    | `Ok map -> of_map ?key map
    | `Duplicate_key key ->
      failwithf "Command.Spec.Arg_type.of_alist_exn: duplicate key %s" key ()
  ;;

  let bool = of_alist_exn [ "true", true; "false", false ]

  let comma_separated
        ?(allow_empty = false)
        ?key
        ?(strip_whitespace = false)
        ?(unique_values = false)
        t
    =
    let strip = if strip_whitespace then fun str -> String.strip str else Fn.id in
    let complete =
      Option.map t.complete ~f:(fun complete_elt env ~part ->
        let prefixes, suffix =
          match String.split part ~on:',' |> List.rev with
          | [] -> [], part
          | hd :: tl -> List.rev tl, hd
        in
        let is_allowed =
          if not unique_values
          then fun (_ : string) -> true
          else (
            let seen_already = prefixes |> List.map ~f:strip |> String.Set.of_list in
            fun choice -> not (Set.mem seen_already (strip choice)))
        in
        let choices =
          match
            List.filter (complete_elt env ~part:suffix) ~f:(fun choice ->
              (not (String.mem choice ',')) && is_allowed choice)
          with
          (* If there is exactly one choice to auto-complete, add a second choice with
             a trailing comma so that auto-completion will go to the end but bash
             won't add a space.  If there are multiple choices, or a single choice
             that must be final, there is no need to add a dummy option. *)
          | [ choice ] -> [ choice; choice ^ "," ]
          | choices -> choices
        in
        List.map choices ~f:(fun choice ->
          String.concat ~sep:"," (prefixes @ [ choice ])))
    in
    let of_string string =
      let string = strip string in
      if String.is_empty string
      then
        if allow_empty
        then []
        else failwith "Command.Spec.Arg_type.comma_separated: empty list not allowed"
      else
        List.map (String.split string ~on:',') ~f:(fun str ->
          Result.ok_exn (t.parse (strip str)))
    in
    create ?key ?complete of_string
  ;;

  module Export = struct
    let string = string
    let int = int
    let char = char
    let float = float
    let bool = bool
    let date = date
    let percent = percent
    let host_and_port = host_and_port
    let sexp = sexp
    let sexp_conv = sexp_conv
  end
end

module Flag = struct
  module Num_occurrences = struct
    type t = Shape.Num_occurrences.t =
      { at_least_once : bool
      ; at_most_once : bool
      }
    [@@deriving compare, enumerate, fields, sexp_of]

    let to_help_string = Shape.Num_occurrences.to_help_string

    let to_help_string_deprecated { at_least_once; at_most_once = _ } flag_name =
      to_help_string { at_least_once; at_most_once = true } ~flag_name
    ;;

    let any = { at_least_once = false; at_most_once = false }
    let at_least_once = { at_least_once = true; at_most_once = false }
    let at_most_once = { at_least_once = false; at_most_once = true }
    let exactly_once = { at_least_once = true; at_most_once = true }
  end

  type action =
    | No_arg of (Env.t -> Env.t)
    | Arg of (Env.t -> string -> Env.t) * Completer.t
    | Rest of (Env.t -> string list -> Env.t)

  module Internal = struct
    type t =
      { name : string
      ; aliases : string list
      ; aliases_excluded_from_help : string list
      (* [aliases_excluded_from_help] are aliases that don't show up in -help output.
         Currently they're only used for double-dash built-in flags like --help and
         --version. *)
      ; action : action
      ; doc : string
      ; num_occurrences : Num_occurrences.t
      ; check_available : Env.t -> unit
      ; name_matching : [ `Prefix | `Full_match_required ]
      }

    let wrap_if_optional t flag_name =
      Num_occurrences.to_help_string t.num_occurrences ~flag_name
    ;;

    module Doc = struct
      type t =
        { arg_doc : string option
        ; doc : string
        }

      let parse ~action ~doc =
        let arg_doc =
          match (action : action) with
          | No_arg _ -> None
          | Rest _ | Arg _ ->
            (match String.lsplit2 doc ~on:' ' with
             | None | Some ("", _) ->
               (match action with
                | Arg _ -> Some ("_", doc)
                | Rest _ | No_arg _ -> None)
             | Some (arg, doc) -> Some (arg, doc))
        in
        match arg_doc with
        | None -> { doc = String.strip doc; arg_doc = None }
        | Some (arg_doc, doc) -> { doc = String.strip doc; arg_doc = Some arg_doc }
      ;;

      let concat ~name ~arg_doc =
        match arg_doc with
        | None -> name
        | Some arg_doc -> name ^ " " ^ arg_doc
      ;;
    end

    module Deprecated = struct
      let wrap_if_optional t x =
        Num_occurrences.to_help_string_deprecated t.num_occurrences x
      ;;

      (* flag help in the format of the old command. used for injection *)
      let help
            ({ name
             ; doc
             ; aliases
             ; action
             ; num_occurrences = _
             ; check_available = _
             ; name_matching = _
             ; aliases_excluded_from_help = _
             } as t)
        =
        if String.is_prefix doc ~prefix:" "
        then
          (name, String.lstrip doc)
          :: List.map aliases ~f:(fun x -> x, sprintf "same as \"%s\"" name)
        else (
          let { Doc.arg_doc; doc } = Doc.parse ~action ~doc in
          (wrap_if_optional t (Doc.concat ~name ~arg_doc), doc)
          :: List.map aliases ~f:(fun x ->
            ( wrap_if_optional t (Doc.concat ~name:x ~arg_doc)
            , sprintf "same as \"%s\"" name )))
      ;;
    end

    let align
          ({ name
           ; doc
           ; aliases
           ; action
           ; num_occurrences = _
           ; check_available = _
           ; name_matching = _
           ; aliases_excluded_from_help = _
           } as t)
      : Shape.Flag_info.t
      =
      let { Doc.arg_doc; doc } = Doc.parse ~action ~doc in
      let name = wrap_if_optional t (Doc.concat ~name ~arg_doc) in
      { name; doc; aliases }
    ;;

    let create flags =
      match String.Map.of_alist (List.map flags ~f:(fun flag -> flag.name, flag)) with
      | `Duplicate_key flag -> failwithf "multiple flags named %s" flag ()
      | `Ok map ->
        List.concat_map flags ~f:(fun flag -> flag.name :: flag.aliases)
        |> List.find_a_dup ~compare:[%compare: string]
        |> Option.iter ~f:(fun x -> failwithf "multiple flags or aliases named %s" x ());
        map
    ;;
  end

  type 'a state =
    { action : action
    ; read : Env.t -> 'a
    ; num_occurrences : Num_occurrences.t
    }

  type 'a t = string -> 'a state

  let arg_flag name arg_type read write num_occurrences =
    { read
    ; num_occurrences
    ; action =
        (let update env arg =
           match arg_type.Arg_type.parse arg with
           | Error exn ->
             die "failed to parse %s value %S.\n%s" name arg (Exn.to_string exn) ()
           | Ok arg ->
             let env = write env arg in
             (match arg_type.Arg_type.key with
              | None -> env
              | Some key -> Env.multi_add env key arg)
         in
         Arg (update, arg_type.Arg_type.complete))
    }
  ;;

  let map_flag t ~f input =
    let { action; read; num_occurrences } = t input in
    { action; read = (fun env -> f (read env)); num_occurrences }
  ;;

  let write_option name key env arg =
    Env.update env key ~f:(function
      | None -> arg
      | Some _ -> die "flag %s passed more than once" name ())
  ;;

  let required_value ?default arg_type name num_occurrences =
    let key = Env.Key.create ~name [%sexp_of: _] in
    let read env =
      match Env.find env key with
      | Some v -> v
      | None ->
        (match default with
         | Some v -> v
         | None -> die "missing required flag: %s" name ())
    in
    let write env arg = write_option name key env arg in
    arg_flag name arg_type read write num_occurrences
  ;;

  let required arg_type name = required_value arg_type name Num_occurrences.exactly_once

  let optional_with_default default arg_type name =
    required_value ~default arg_type name Num_occurrences.at_most_once
  ;;

  let optional arg_type name =
    let key = Env.Key.create ~name [%sexp_of: _] in
    let read env = Env.find env key in
    let write env arg = write_option name key env arg in
    arg_flag name arg_type read write Num_occurrences.at_most_once
  ;;

  let no_arg_general ~key_value ~deprecated_hook name =
    let key = Env.Key.create ~name [%sexp_of: unit] in
    let read env = Env.mem env key in
    let write env =
      if Env.mem env key
      then die "flag %s passed more than once" name ()
      else Env.set env key ()
    in
    let action env =
      let env =
        Option.fold key_value ~init:env ~f:(fun env (key, value) ->
          Env.set_with_default env key value)
      in
      write env
    in
    let action =
      match deprecated_hook with
      | None -> action
      | Some f ->
        fun env ->
          let env = action env in
          f ();
          env
    in
    { read; action = No_arg action; num_occurrences = Num_occurrences.at_most_once }
  ;;

  let no_arg name = no_arg_general name ~key_value:None ~deprecated_hook:None

  let no_arg_register ~key ~value name =
    no_arg_general name ~key_value:(Some (key, value)) ~deprecated_hook:None
  ;;

  let no_arg_some value =
    map_flag no_arg ~f:(function
      | true -> Some value
      | false -> None)
  ;;

  let listed arg_type name =
    let key = Env.With_default.Key.create ~default:[] ~name [%sexp_of: _ list] in
    let read env = List.rev (Env.With_default.find env key) in
    let write env arg = Env.With_default.change env key ~f:(fun list -> arg :: list) in
    arg_flag name arg_type read write Num_occurrences.any
  ;;

  let one_or_more arg_type name =
    let key =
      Env.With_default.Key.create ~default:Fqueue.empty ~name [%sexp_of: _ Fqueue.t]
    in
    let read env =
      match Fqueue.to_list (Env.With_default.find env key) with
      | first :: rest -> first, rest
      | [] -> die "missing required flag: %s" name ()
    in
    let write env arg =
      Env.With_default.change env key ~f:(fun q -> Fqueue.enqueue q arg)
    in
    arg_flag name arg_type read write Num_occurrences.at_least_once
  ;;

  let escape_general ~deprecated_hook name =
    let key = Env.Key.create ~name [%sexp_of: string list] in
    let action env cmd_line = Env.set env key cmd_line in
    let read env = Env.find env key in
    let action =
      match deprecated_hook with
      | None -> action
      | Some f ->
        fun env x ->
          f x;
          action env x
    in
    { action = Rest action; read; num_occurrences = Num_occurrences.at_most_once }
  ;;

  let no_arg_abort ~exit _name =
    { action = No_arg (fun _ -> never_returns (exit ()))
    ; num_occurrences = Num_occurrences.at_most_once
    ; read = (fun _ -> ())
    }
  ;;

  let escape name = escape_general ~deprecated_hook:None name

  module Deprecated = struct
    let no_arg ~hook name =
      no_arg_general ~deprecated_hook:(Some hook) ~key_value:None name
    ;;

    let escape ~hook = escape_general ~deprecated_hook:(Some hook)
  end
end

module Path : sig
  type t

  val empty : t
  val create : path_to_exe:string -> t
  val of_parts : string list -> t
  val append : t -> subcommand:string -> t
  val replace_first : t -> from:string -> to_:string -> t
  val parts : t -> string list
  val parts_exe_basename : t -> string list
  val to_string : t -> string
  val to_string_dots : t -> string
  val pop_help : t -> t
  val length : t -> int
  val is_empty : t -> bool
end = struct
  type t = string list

  let empty = []
  let create ~path_to_exe = [ path_to_exe ]
  let of_parts parts = List.rev parts
  let append t ~subcommand = subcommand :: t
  let parts = List.rev

  let parts_exe_basename t =
    match List.rev t with
    | [] -> []
    | hd :: tl -> Filename.basename hd :: tl
  ;;

  let to_string t = unwords (parts_exe_basename t)
  let length = List.length

  let replace_first t ~from ~to_ =
    let rec aux parts ~acc ~from ~to_ =
      match parts with
      | [] -> acc
      | hd :: tl ->
        if String.( = ) hd from
        then List.rev_append tl (to_ :: acc)
        else aux tl ~acc:(hd :: acc) ~from ~to_
    in
    aux (parts t) ~acc:[] ~from ~to_
  ;;

  let pop_help = function
    | "help" :: t -> t
    | _ -> assert false
  ;;

  let to_string_dots t =
    (match t with
     | [] -> []
     | last :: init -> last :: List.map init ~f:(Fn.const "."))
    |> to_string
  ;;

  let is_empty = List.is_empty
end

module Anons = struct
  module Grammar : sig
    type t = Shape.Anons.Grammar.t

    val zero : t
    val one : string -> t
    val many : t -> t
    val maybe : t -> t
    val concat : t list -> t
    val ad_hoc : usage:string -> t

    include Invariant.S with type t := t

    val names : t -> string list
  end = struct
    type t = Shape.Anons.Grammar.t =
      | Zero
      | One of string
      | Many of t
      | Maybe of t
      | Concat of t list
      | Ad_hoc of string

    let invariant = Shape.Anons.Grammar.invariant
    let usage = Shape.Anons.Grammar.usage

    let rec is_fixed_arity = function
      | Zero -> true
      | One _ -> true
      | Many _ -> false
      | Maybe _ -> false
      | Ad_hoc _ -> false
      | Concat ts ->
        (match List.rev ts with
         | [] -> failwith "bug in command.ml"
         | last :: others ->
           assert (List.for_all others ~f:is_fixed_arity);
           is_fixed_arity last)
    ;;

    let rec names = function
      | Zero -> []
      | One s -> [ s ]
      | Many t -> names t
      | Maybe t -> names t
      | Ad_hoc s -> [ s ]
      | Concat ts -> List.concat_map ts ~f:names
    ;;

    let zero = Zero
    let one name = One name

    let many = function
      | Zero -> Zero (* strange, but not non-sense *)
      | t ->
        if not (is_fixed_arity t)
        then
          failwithf
            "iteration of variable-length grammars such as %s is disallowed"
            (usage t)
            ();
        Many t
    ;;

    let maybe = function
      | Zero -> Zero (* strange, but not non-sense *)
      | t -> Maybe t
    ;;

    let concat = function
      | [] -> Zero
      | car :: cdr ->
        let car, cdr =
          List.fold cdr ~init:(car, []) ~f:(fun (t1, acc) t2 ->
            match t1, t2 with
            | Zero, t | t, Zero -> t, acc
            | _, _ ->
              if is_fixed_arity t1
              then t2, t1 :: acc
              else
                failwithf
                  "the grammar %s for anonymous arguments is not supported because \
                   there is the possibility for arguments (%s) following a variable \
                   number of arguments (%s).  Supporting such grammars would \
                   complicate the implementation significantly."
                  (usage (Concat (List.rev (t2 :: t1 :: acc))))
                  (usage t2)
                  (usage t1)
                  ())
        in
        (match cdr with
         | [] -> car
         | _ :: _ -> Concat (List.rev (car :: cdr)))
    ;;

    let ad_hoc ~usage = Ad_hoc usage
  end

  module Parser : sig
    type +'a t

    val from_env : (Env.t -> 'a) -> 'a t
    val one : name:string -> 'a Arg_type.t -> 'a t
    val maybe : 'a t -> 'a option t
    val sequence : 'a t -> 'a list t
    val final_value : 'a t -> Env.t -> 'a
    val consume : 'a t -> string -> for_completion:bool -> (Env.t -> Env.t) * 'a t
    val complete : 'a t -> Env.t -> part:string -> never_returns

    module For_opening : sig
      val return : 'a -> 'a t
      val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
      val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    end
  end = struct
    type 'a t =
      | Done of (Env.t -> 'a)
      | More of 'a more
      (* A [Test] will (generally) return a [Done _] value if there is no more input and
         a [More] parser to use if there is any more input. *)
      | Test of (more:bool -> 'a t)
      (* If we're only completing, we can't pull values out, but we can still step through
         [t]s (which may have completion set up). *)
      | Only_for_completion of packed list

    and 'a more =
      { name : string
      ; parse : string -> for_completion:bool -> (Env.t -> Env.t) * 'a t
      ; complete : Completer.t
      }

    and packed = Packed : 'a t -> packed

    let return a = Done (fun _ -> a)
    let from_env f = Done f

    let pack_for_completion = function
      | Done _ -> [] (* won't complete or consume anything *)
      | (More _ | Test _) as x -> [ Packed x ]
      | Only_for_completion ps -> ps
    ;;

    let rec ( <*> ) tf tx =
      match tf with
      | Done f ->
        (match tx with
         | Done x -> Done (fun env -> f env (x env))
         | Test test -> Test (fun ~more -> tf <*> test ~more)
         | More { name; parse; complete } ->
           let parse arg ~for_completion =
             let upd, tx' = parse arg ~for_completion in
             upd, tf <*> tx'
           in
           More { name; parse; complete }
         | Only_for_completion packed -> Only_for_completion packed)
      | Test test -> Test (fun ~more -> test ~more <*> tx)
      | More { name; parse; complete } ->
        let parse arg ~for_completion =
          let upd, tf' = parse arg ~for_completion in
          upd, tf' <*> tx
        in
        More { name; parse; complete }
      | Only_for_completion packed ->
        Only_for_completion (packed @ pack_for_completion tx)
    ;;

    let ( >>| ) t f = return f <*> t

    let one_more ~name { Arg_type.complete; parse = of_string; key } =
      let parse anon ~for_completion =
        match of_string anon with
        | Error exn ->
          if for_completion
          then
            (* we don't *really* care about this value, so just put in a dummy value so
               completion can continue *)
            Fn.id, Only_for_completion []
          else die "failed to parse %s value %S\n%s" name anon (Exn.to_string exn) ()
        | Ok v ->
          let update env =
            Option.fold key ~init:env ~f:(fun env key -> Env.multi_add env key v)
          in
          update, return v
      in
      More { name; parse; complete }
    ;;

    let one ~name arg_type =
      Test
        (fun ~more ->
           if more
           then one_more ~name arg_type
           else die "missing anonymous argument: %s" name ())
    ;;

    let maybe t = Test (fun ~more -> if more then t >>| fun a -> Some a else return None)

    let sequence t =
      let rec loop =
        Test
          (fun ~more ->
             if more then return (fun v acc -> v :: acc) <*> t <*> loop else return [])
      in
      loop
    ;;

    let rec final_value t env =
      match t with
      | Done a -> a env
      | Test f -> final_value (f ~more:false) env
      | More { name; _ } -> die "missing anonymous argument: %s" name ()
      | Only_for_completion _ ->
        failwith "BUG: asked for final value when doing completion"
    ;;

    let rec consume
      : type a. a t -> string -> for_completion:bool -> (Env.t -> Env.t) * a t
      =
      fun t arg ~for_completion ->
        match t with
        | Done _ -> die "too many anonymous arguments" ()
        | Test f -> consume (f ~more:true) arg ~for_completion
        | More { parse; _ } -> parse arg ~for_completion
        | Only_for_completion packed ->
          (match packed with
           | [] -> Fn.id, Only_for_completion []
           | Packed t :: rest ->
             let upd, t = consume t arg ~for_completion in
             upd, Only_for_completion (pack_for_completion t @ rest))
    ;;

    let rec complete : type a. a t -> Env.t -> part:string -> never_returns =
      fun t env ~part ->
        match t with
        | Done _ -> exit 0
        | Test f -> complete (f ~more:true) env ~part
        | More { complete; _ } -> Completer.run_and_exit complete env ~part
        | Only_for_completion t ->
          (match t with
           | [] -> exit 0
           | Packed t :: _ -> complete t env ~part)
    ;;

    module For_opening = struct
      let return = return
      let ( <*> ) = ( <*> )
      let ( >>| ) = ( >>| )
    end
  end

  open Parser.For_opening

  type 'a t =
    { p : 'a Parser.t
    ; grammar : Grammar.t
    }

  let t2 t1 t2 =
    { p = return (fun a1 a2 -> a1, a2) <*> t1.p <*> t2.p
    ; grammar = Grammar.concat [ t1.grammar; t2.grammar ]
    }
  ;;

  let t3 t1 t2 t3 =
    { p = return (fun a1 a2 a3 -> a1, a2, a3) <*> t1.p <*> t2.p <*> t3.p
    ; grammar = Grammar.concat [ t1.grammar; t2.grammar; t3.grammar ]
    }
  ;;

  let t4 t1 t2 t3 t4 =
    { p = return (fun a1 a2 a3 a4 -> a1, a2, a3, a4) <*> t1.p <*> t2.p <*> t3.p <*> t4.p
    ; grammar = Grammar.concat [ t1.grammar; t2.grammar; t3.grammar; t4.grammar ]
    }
  ;;

  let normalize str =
    (* Verify the string is not empty or surrounded by whitespace *)
    let strlen = String.length str in
    if strlen = 0 then failwith "Empty anonymous argument name provided";
    if String.( <> ) (String.strip str) str
    then failwithf "argument name %S has surrounding whitespace" str ();
    (* If the string contains special surrounding characters, don't do anything *)
    let has_special_chars =
      let special_chars = Char.Set.of_list [ '<'; '>'; '['; ']'; '('; ')'; '{'; '}' ] in
      String.exists str ~f:(Set.mem special_chars)
    in
    if has_special_chars then str else String.uppercase str
  ;;

  let ( %: ) name arg_type =
    let name = normalize name in
    { p = Parser.one ~name arg_type; grammar = Grammar.one name }
  ;;

  let map_anons t ~f = { p = t.p >>| f; grammar = t.grammar }
  let maybe t = { p = Parser.maybe t.p; grammar = Grammar.maybe t.grammar }

  let maybe_with_default default t =
    let t = maybe t in
    { t with p = (t.p >>| fun v -> Option.value ~default v) }
  ;;

  let sequence t = { p = Parser.sequence t.p; grammar = Grammar.many t.grammar }
  let non_empty_sequence_as_pair t = t2 t (sequence t)

  let non_empty_sequence_as_list t =
    let t = non_empty_sequence_as_pair t in
    { t with p = (t.p >>| fun (x, xs) -> x :: xs) }
  ;;

  module Deprecated = struct
    let ad_hoc ~usage_arg =
      { p = Parser.sequence (Parser.one ~name:"WILL NEVER BE PRINTED" Arg_type.string)
      ; grammar = Grammar.ad_hoc ~usage:usage_arg
      }
    ;;
  end
end

module Cmdline = struct
  type t =
    | Nil
    | Cons of string * t
    | Complete of string
  [@@deriving compare]

  let of_list args = List.fold_right args ~init:Nil ~f:(fun arg args -> Cons (arg, args))

  let rec to_list = function
    | Nil -> []
    | Cons (x, xs) -> x :: to_list xs
    | Complete x -> [ x ]
  ;;

  let rec ends_in_complete = function
    | Complete _ -> true
    | Nil -> false
    | Cons (_, args) -> ends_in_complete args
  ;;

  let extend t ~extend ~path =
    if ends_in_complete t
    then t
    else (
      let path_list = Option.value ~default:[] (List.tl (Path.parts path)) in
      of_list (to_list t @ extend path_list))
  ;;
end

module Key_type = Shape.Private.Key_type

let assert_no_underscores key_type flag_or_subcommand =
  if String.exists flag_or_subcommand ~f:(fun c -> Char.( = ) c '_')
  then
    failwithf
      "%s %s contains an underscore. Use a dash instead."
      (Key_type.to_string key_type)
      flag_or_subcommand
      ()
;;

let normalize key_type key =
  assert_no_underscores key_type key;
  match key_type with
  | Key_type.Flag ->
    if String.equal key "-"
    then failwithf !"invalid %{Key_type} name: %S" key_type key ();
    if String.exists key ~f:Char.is_whitespace
    then failwithf !"invalid %{Key_type} name (contains whitespace): %S" key_type key ();
    if String.is_prefix ~prefix:"-" key then key else "-" ^ key
  | Key_type.Subcommand -> String.lowercase key
;;

let lookup_expand = Shape.Private.lookup_expand

let lookup_expand_with_aliases map prefix key_type =
  let alist =
    List.concat_map (String.Map.data map) ~f:(fun flag ->
      let { Flag.Internal.name
          ; aliases
          ; aliases_excluded_from_help
          ; action = _
          ; doc = _
          ; num_occurrences = _
          ; check_available = _
          ; name_matching
          }
        =
        flag
      in
      let data = flag, name_matching in
      let aliases = aliases_excluded_from_help @ aliases in
      (name, data) :: List.map aliases ~f:(fun alias -> alias, data))
  in
  match List.find_a_dup alist ~compare:(fun (s1, _) (s2, _) -> String.compare s1 s2) with
  | None -> lookup_expand alist prefix key_type
  | Some (flag, _) -> failwithf "multiple flags named %s" flag ()
;;

module Base = struct
  type t =
    { summary : string
    ; readme : (unit -> string) option
    ; flags : Flag.Internal.t String.Map.t
    ;
      anons : unit -> ([ `Parse_args ] -> [ `Run_main ] -> unit) Anons.Parser.t
    ; usage : Anons.Grammar.t
    }

  module Deprecated = struct
    let subcommand_cmp_fst (a, _) (c, _) = help_screen_compare a c

    let flags_help ?(display_help_flags = true) t =
      let flags = String.Map.data t.flags in
      let flags =
        if display_help_flags
        then flags
        else List.filter flags ~f:(fun f -> String.( <> ) f.name "-help")
      in
      List.concat_map ~f:Flag.Internal.Deprecated.help flags
    ;;
  end

  let formatted_flags t =
    String.Map.data t.flags
    |> List.map ~f:Flag.Internal.align
    (* this sort puts optional flags after required ones *)
    |> List.sort ~compare:(fun a b -> String.compare a.name b.name)
    |> Shape.Flag_info.sort
  ;;

  let shape t : Shape.Base_info.t =
    { summary = t.summary
    ; readme = Option.map t.readme ~f:(fun readme -> readme ())
    ; anons = Grammar t.usage
    ; flags = formatted_flags t
    }
  ;;

  let path_key = Env.key_create "path"
  let args_key = Env.key_create "args"
  let help_key = Env.key_create "help"

  let indent_by_2 str =
    String.split ~on:'\n' str
    |> List.map ~f:(fun line -> "  " ^ line)
    |> String.concat ~sep:"\n"
  ;;

  let run t env ~when_parsing_succeeds ~path ~args ~verbose_on_parse_error ~help_text =
    let env = Env.set env path_key path in
    let env = Env.set env args_key (Cmdline.to_list args) in
    let env = Env.set env help_key help_text in
    let rec loop env anons = function
      | Cmdline.Nil ->
        List.iter (String.Map.data t.flags) ~f:(fun flag -> flag.check_available env);
        Anons.Parser.final_value anons env
      | Cons ("-anon", Cons (arg, args)) ->
        (* the very special -anon flag is here as an escape hatch in case you have an
           anonymous argument that starts with a hyphen. *)
        anon env anons arg args
      | Cons (arg, args) ->
        if String.is_prefix arg ~prefix:"-" && not (String.equal arg "-")
        (* support the convention where "-" means stdin *)
        then (
          let flag = arg in
          let ( flag
              , { Flag.Internal.action
                ; name = _
                ; aliases = _
                ; aliases_excluded_from_help = _
                ; doc = _
                ; num_occurrences = _
                ; check_available = _
                ; name_matching = _
                } )
            =
            match lookup_expand_with_aliases t.flags flag Key_type.Flag with
            | Error msg -> die "%s" msg ()
            | Ok x -> x
          in
          match action with
          | No_arg f ->
            let env = f env in
            loop env anons args
          | Arg (f, comp) ->
            (match args with
             | Nil -> die "missing argument for flag %s" flag ()
             | Cons (arg, rest) ->
               let env =
                 try f env arg with
                 | Failed_to_parse_command_line _ as e ->
                   if Cmdline.ends_in_complete rest then env else raise e
               in
               loop env anons rest
             | Complete part -> never_returns (Completer.run_and_exit comp env ~part))
          | Rest f ->
            if Cmdline.ends_in_complete args then exit 0;
            let env = f env (Cmdline.to_list args) in
            loop env anons Nil)
        else anon env anons arg args
      | Complete part ->
        if String.is_prefix part ~prefix:"-"
        then (
          List.iter (String.Map.keys t.flags) ~f:(fun name ->
            if String.is_prefix name ~prefix:part then print_endline name);
          exit 0)
        else never_returns (Anons.Parser.complete anons env ~part)
    and anon env anons arg args =
      let env_upd, anons =
        Anons.Parser.consume anons arg ~for_completion:(Cmdline.ends_in_complete args)
      in
      let env = env_upd env in
      loop env anons args
    in
    match Result.try_with (fun () -> loop env (t.anons ()) args `Parse_args) with
    | Ok thunk ->
      when_parsing_succeeds ();
      thunk `Run_main
    | Error exn ->
      (match exn with
       | Failed_to_parse_command_line _ when Cmdline.ends_in_complete args -> exit 0
       | Exit_called { status } -> exit status
       | _ ->
         let exn_str =
           match exn with
           | Failed_to_parse_command_line msg -> msg
           | _ -> Sexp.to_string_hum [%sexp (exn : exn)]
         in
         let verbose = Option.value verbose_on_parse_error ~default:true in
         let error_msg =
           if verbose
           then
             String.concat
               ~sep:"\n\n"
               [ "Error parsing command line:"
               ; indent_by_2 exn_str
               ; "For usage information, run"
               ; "  " ^ Path.to_string path ^ " -help\n"
               ]
           else exn_str
         in
         prerr_endline error_msg;
         exit 1)
  ;;

  module Spec = struct
    type ('a, 'b) t =
      { f : unit -> ('a -> 'b) Anons.Parser.t
      ; usage : unit -> Anons.Grammar.t
      ; flags : unit -> Flag.Internal.t list
      }

    (* the (historical) reason that [param] is defined in terms of [t] rather than the
       other way round is that the delayed evaluation mattered for sequencing of
       read/write operations on ref cells in the old representation of flags *)
    type 'a param = { param : 'm. ('a -> 'm, 'm) t }

    open Anons.Parser.For_opening

    let app t1 t2 ~f =
      { f = (fun () -> return f <*> t1.f () <*> t2.f ())
      ; flags = (fun () -> t2.flags () @ t1.flags ())
      ; usage = (fun () -> Anons.Grammar.concat [ t1.usage (); t2.usage () ])
      }
    ;;

    (* So sad.  We can't define [apply] in terms of [app] because of the value
       restriction. *)
    let apply pf px =
      { param =
          { f =
              (fun () ->
                 return (fun mf mx k -> mf (fun f -> mx (fun x -> k (f x))))
                 <*> pf.param.f ()
                 <*> px.param.f ())
          ; flags = (fun () -> px.param.flags () @ pf.param.flags ())
          ; usage =
              (fun () -> Anons.Grammar.concat [ pf.param.usage (); px.param.usage () ])
          }
      }
    ;;

    let ( ++ ) t1 t2 = app t1 t2 ~f:(fun f1 f2 x -> f2 (f1 x))
    let ( +> ) t1 p2 = app t1 p2.param ~f:(fun f1 f2 x -> f2 (f1 x))
    let ( +< ) t1 p2 = app p2.param t1 ~f:(fun f2 f1 x -> f1 (f2 x))

    let step f =
      { f = (fun () -> return f)
      ; flags = (fun () -> [])
      ; usage = (fun () -> Anons.Grammar.zero)
      }
    ;;

    let empty : 'm. ('m, 'm) t =
      { f = (fun () -> return Fn.id)
      ; flags = (fun () -> [])
      ; usage = (fun () -> Anons.Grammar.zero)
      }
    ;;

    let const v =
      { param =
          { f = (fun () -> return (fun k -> k v))
          ; flags = (fun () -> [])
          ; usage = (fun () -> Anons.Grammar.zero)
          }
      }
    ;;

    let map p ~f =
      { param =
          { f = (fun () -> p.param.f () >>| fun c k -> c (fun v -> k (f v)))
          ; flags = p.param.flags
          ; usage = p.param.usage
          }
      }
    ;;

    let wrap f t =
      { f = (fun () -> t.f () >>| fun run main -> f ~run ~main)
      ; flags = t.flags
      ; usage = t.usage
      }
    ;;

    let of_params params =
      let t = params.param in
      { f = (fun () -> t.f () >>| fun run main -> run Fn.id main)
      ; flags = t.flags
      ; usage = t.usage
      }
    ;;

    let to_params (t : ('a, 'b) t) : ('a -> 'b) param =
      { param =
          { f = (fun () -> t.f () >>| fun f k -> k f); flags = t.flags; usage = t.usage }
      }
    ;;

    let of_param p = p.param
    let to_param t main = map (to_params t) ~f:(fun k -> k main)

    let lookup key =
      { param =
          { f = (fun () -> Anons.Parser.from_env (fun env m -> m (Env.find_exn env key)))
          ; flags = (fun () -> [])
          ; usage = (fun () -> Anons.Grammar.zero)
          }
      }
    ;;

    let path : Path.t param = lookup path_key
    let args : string list param = lookup args_key
    let help : string Lazy.t param = lookup help_key

    (* This is only used internally, for the help command. *)
    let env =
      { param =
          { f = (fun () -> Anons.Parser.from_env (fun env m -> m env))
          ; flags = (fun () -> [])
          ; usage = (fun () -> Anons.Grammar.zero)
          }
      }
    ;;

    include struct
      module Arg_type = Arg_type
      include Arg_type.Export
    end

    include struct
      open Anons

      type 'a anons = 'a t

      let ( %: ) = ( %: )
      let map_anons = map_anons
      let maybe = maybe
      let maybe_with_default = maybe_with_default
      let sequence = sequence
      let non_empty_sequence_as_pair = non_empty_sequence_as_pair
      let non_empty_sequence_as_list = non_empty_sequence_as_list
      let t2 = t2
      let t3 = t3
      let t4 = t4

      let anon spec =
        Anons.Grammar.invariant spec.grammar;
        { param =
            { f = (fun () -> spec.p >>| fun v k -> k v)
            ; flags = (fun () -> [])
            ; usage = (fun () -> spec.grammar)
            }
        }
      ;;
    end

    include struct
      open Flag

      type 'a flag = 'a t

      let map_flag = map_flag
      let escape = escape
      let listed = listed
      let one_or_more = one_or_more
      let no_arg = no_arg
      let no_arg_register = no_arg_register
      let no_arg_abort = no_arg_abort
      let no_arg_some = no_arg_some
      let optional = optional
      let optional_with_default = optional_with_default
      let required = required

      let flag_internal
            ?(aliases = [])
            ?full_flag_required
            name
            mode
            ~doc
            ~aliases_excluded_from_help
        =
        let normalize flag = normalize Key_type.Flag flag in
        let name = normalize name in
        let aliases = List.map ~f:normalize aliases in
        let { read; action; num_occurrences } = mode name in
        let check_available =
          match num_occurrences.at_least_once with
          | false -> (ignore : Univ_map.t -> unit)
          | true -> fun env -> ignore (read env : _)
        in
        let name_matching =
          if Option.is_some full_flag_required then `Full_match_required else `Prefix
        in
        { param =
            { f = (fun () -> Anons.Parser.from_env (fun env m -> m (read env)))
            ; flags =
                (fun () ->
                   [ { name
                     ; aliases
                     ; aliases_excluded_from_help
                     ; doc
                     ; action
                     ; num_occurrences
                     ; check_available
                     ; name_matching
                     }
                   ])
            ; usage = (fun () -> Anons.Grammar.zero)
            }
        }
      ;;

      let flag = flag_internal ~aliases_excluded_from_help:[]

      let flag_optional_with_default_doc
            ?aliases
            ?full_flag_required
            name
            arg_type
            sexp_of_default
            ~default
            ~doc
        =
        flag
          ?aliases
          ?full_flag_required
          name
          (optional_with_default default arg_type)
          ~doc:(sprintf !"%s (default: %{Sexp})" doc (sexp_of_default default))
      ;;

      include Applicative.Make (struct
          type nonrec 'a t = 'a param

          let return = const
          let apply = apply
          let map = `Custom map
        end)

      let pair = both
    end

    let flags_of_args_exn args =
      List.fold args ~init:empty ~f:(fun acc (name, spec, doc) ->
        let gen f flag_type =
          step (fun m x ->
            f x;
            m)
          +> flag name flag_type ~doc
        in
        let call f arg_type = gen (fun x -> Option.iter x ~f) (optional arg_type) in
        let set r arg_type = call (fun x -> r := x) arg_type in
        let set_bool r b = gen (fun passed -> if passed then r := b) no_arg in
        acc
        ++
        match spec with
        | Arg.Unit f -> gen (fun passed -> if passed then f ()) no_arg
        | Arg.Set r -> set_bool r true
        | Arg.Clear r -> set_bool r false
        | Arg.String f -> call f string
        | Arg.Set_string r -> set r string
        | Arg.Int f -> call f int
        | Arg.Set_int r -> set r int
        | Arg.Float f -> call f float
        | Arg.Set_float r -> set r float
        | Arg.Bool f -> call f bool
        | Arg.Symbol (syms, f) ->
          let arg_type =
            Arg_type.of_alist_exn (List.map syms ~f:(fun sym -> sym, sym))
          in
          call f arg_type
        | Arg.Rest f -> gen (fun x -> Option.iter x ~f:(List.iter ~f)) escape
        | Arg.Tuple _ ->
          failwith "Arg.Tuple is not supported by Command.Spec.flags_of_args_exn"
        | ((Arg.Expand _)[@if ocaml_version >= (4, 05, 0)]) ->
          failwith "Arg.Expand is not supported by Command.Spec.flags_of_args_exn"
        | ((Arg.Rest_all _)[@if ocaml_version >= (4, 12, 0)]) ->
          failwith "Arg.Rest_all is not supported by Command.Spec.flags_of_args_exn")
    ;;

    module Deprecated = struct
      include Flag.Deprecated
      include Anons.Deprecated
    end

    let arg_names param =
      let t = param.param in
      let flag_names = Map.keys (Flag.Internal.create (t.flags ())) in
      let anon_names = Anons.Grammar.names (t.usage ()) in
      List.concat [ flag_names; anon_names ]
    ;;

    module Choose_one = struct
      module Choice_name : sig
        type t [@@deriving compare, sexp_of]

        include Comparator.S with type t := t

        val to_string : t -> string
        val list_to_string : t list -> string
        val create_exn : 'a param -> t
      end = struct
        module T = struct
          type t = string list [@@deriving compare, sexp_of]
        end

        include T
        include Comparator.Make (T)

        let create_exn param =
          let names = arg_names param in
          let names_with_commas =
            List.filter names ~f:(fun s -> String.contains s ',')
          in
          if not (List.is_empty names_with_commas)
          then
            failwiths
              ~here:[%here]
              "For simplicity, [Command.Spec.choose_one] does not support names with \
               commas."
              names_with_commas
              [%sexp_of: string list];
          match names with
          | [] ->
            raise_s
              [%message "[choose_one] expects choices to read command-line arguments."]
          | _ :: _ -> names
        ;;

        let to_string = String.concat ~sep:","
        let list_to_string ts = List.map ts ~f:to_string |> String.concat ~sep:"\n  "
      end

      module If_nothing_chosen = struct
        type (_, _) t =
          | Default_to : 'a -> ('a, 'a) t
          | Raise : ('a, 'a) t
          | Return_none : ('a, 'a option) t
      end

      let choose_one
            (type a b)
            (ts : a option param list)
            ~(if_nothing_chosen : (a, b) If_nothing_chosen.t)
        =
        match
          List.map ts ~f:(fun t -> Choice_name.create_exn t, t)
          |> Map.of_alist (module Choice_name)
        with
        | `Duplicate_key name ->
          failwiths
            ~here:[%here]
            "[Command.Spec.choose_one] called with duplicate name"
            name
            [%sexp_of: Choice_name.t]
        | `Ok ts ->
          Map.fold ts ~init:(return []) ~f:(fun ~key:name ~data:t init ->
            map2 init t ~f:(fun init value ->
              Option.fold value ~init ~f:(fun init value -> (name, value) :: init)))
          |> map ~f:(fun value : b -> match value with
            | _ :: _ :: _ as passed ->
              die
                !"Cannot pass more than one of these: \n\
                 \  %{Choice_name.list_to_string}"
                (List.map passed ~f:fst)
                ()
            | [ (_, value) ] ->
              (match if_nothing_chosen with
               | Default_to (_ : a) -> (value : b)
               | Raise -> value
               | Return_none -> Some value)
            | [] ->
              (match if_nothing_chosen with
               | Default_to value -> value
               | Return_none -> None
               | Raise ->
                 die
                   !"Must pass one of these:\n  %{Choice_name.list_to_string}"
                   (Map.keys ts)
                   ()))
      ;;
    end

    module If_nothing_chosen = Choose_one.If_nothing_chosen

    let choose_one = Choose_one.choose_one
    let and_arg_names t = map t ~f:(fun value -> value, arg_names t)

    let and_arg_name t =
      match arg_names t with
      | [ name ] -> map t ~f:(fun value -> value, name)
      | names ->
        raise_s
          [%message
            "[and_arg_name] expects exactly one name, got" ~_:(names : string list)]
    ;;
  end
end

module Group = struct
  type 'a t =
    { summary : string
    ; readme : (unit -> string) option
    ; subcommands : (string * 'a) list Lazy.t
    ; body : (path:string list -> unit) option
    }

  let shape ~subcommand_to_shape t : _ Shape.Group_info.t =
    { summary = t.summary
    ; readme = Option.map ~f:(fun readme -> readme ()) t.readme
    ; subcommands = Lazy.map t.subcommands ~f:(List.Assoc.map ~f:subcommand_to_shape)
    }
  ;;
end

let abs_path = Shape.Private.abs_path
let comp_cword = "COMP_CWORD"

module Exec = struct
  type t =
    { summary : string
    ; readme : (unit -> string) option
    ; (* If [path_to_exe] is relative, interpret w.r.t. [working_dir] *)
      working_dir : string
    ; path_to_exe : string
    ; child_subcommand : string list
    }

  let shape t : Shape.Exec_info.t =
    { summary = t.summary
    ; readme = Option.map ~f:(fun readme -> readme ()) t.readme
    ; working_dir = t.working_dir
    ; path_to_exe = t.path_to_exe
    ; child_subcommand = t.child_subcommand
    }
  ;;
end

(* A proxy command is the structure of an Exec command obtained by running it in a
   special way *)
module Proxy = struct
  module Kind = struct
    type 'a t =
      | Base of Shape.Base_info.t
      | Group of 'a Shape.Group_info.t
      | Exec of Shape.Exec_info.t
      | Lazy of 'a t Lazy.t
  end

  type t =
    { working_dir : string
    ; path_to_exe : string
    ; path_to_subcommand : string list
    ; child_subcommand : string list
    ; kind : t Kind.t
    }

  let rec get_summary_from_kind (kind : t Kind.t) =
    match kind with
    | Base b -> b.summary
    | Group g -> g.summary
    | Exec e -> e.summary
    | Lazy l -> get_summary_from_kind (Lazy.force l)
  ;;

  let get_summary t = get_summary_from_kind t.kind

  let rec get_readme_from_kind (kind : t Kind.t) =
    match kind with
    | Base b -> b.readme
    | Group g -> g.readme
    | Exec e -> e.readme
    | Lazy l -> get_readme_from_kind (Lazy.force l)
  ;;

  let get_readme t = get_readme_from_kind t.kind
end

type t =
  | Base of Base.t
  | Group of t Group.t
  | Exec of Exec.t
  | Proxy of Proxy.t
  | Lazy of t Lazy.t

let rec sexpable_of_proxy_kind (kind : Proxy.t Proxy.Kind.t) : Shape.Sexpable.t =
  match kind with
  | Base base -> Base base
  | Exec exec -> Exec exec
  | Lazy thunk -> Lazy (Lazy.map ~f:sexpable_of_proxy_kind thunk)
  | Group group ->
    Group
      { group with
        subcommands =
          Lazy.map
            group.subcommands
            ~f:
              (List.map ~f:(fun (str, proxy) ->
                 str, sexpable_of_proxy_kind proxy.Proxy.kind))
      }
;;

let sexpable_of_proxy proxy = sexpable_of_proxy_kind proxy.Proxy.kind

let rec sexpable_shape : t -> Shape.Sexpable.t = function
  | Base base -> Base (Base.shape base)
  | Exec exec -> Exec (Exec.shape exec)
  | Proxy proxy -> sexpable_of_proxy proxy
  | Group group -> Group (Group.shape ~subcommand_to_shape:sexpable_shape group)
  | Lazy thunk -> Lazy (Lazy.map ~f:sexpable_shape thunk)
;;

type ('main, 'result) basic_spec_command =
  summary:string
  -> ?readme:(unit -> string)
  -> ('main, unit -> 'result) Base.Spec.t
  -> 'main
  -> t

let extend_exn ~mem ~add map key_type ~key data =
  if mem map key
  then failwithf "there is already a %s named %s" (Key_type.to_string key_type) key ();
  add map ~key ~data
;;

let extend_map_exn map key_type ~key data =
  extend_exn map key_type ~key data ~mem:Map.mem ~add:Map.set
;;

let extend_alist_exn alist key_type ~key data =
  extend_exn
    alist
    key_type
    ~key
    data
    ~mem:(fun alist key -> List.Assoc.mem alist key ~equal:String.equal)
    ~add:(fun alist ~key ~data -> List.Assoc.add alist key data ~equal:String.equal)
;;

module Bailout_dump_flag = struct
  let add base ~name ~aliases ~aliases_excluded_from_help ~text ~text_summary =
    let flags = base.Base.flags in
    let flags =
      extend_map_exn
        flags
        Key_type.Flag
        ~key:name
        { name
        ; aliases_excluded_from_help
        ; aliases
        ; num_occurrences = Flag.Num_occurrences.at_most_once
        ; check_available = ignore
        ; action =
            No_arg
              (fun env ->
                 print_endline (text env);
                 exit 0)
        ; doc = sprintf " print %s and exit" text_summary
        ; name_matching = `Prefix
        }
    in
    { base with Base.flags }
  ;;
end

let basic_spec ~summary ?readme { Base.Spec.usage; flags; f } main =
  let flags = flags () in
  let usage = usage () in
  let anons () =
    let open Anons.Parser.For_opening in
    f ()
    >>| fun k `Parse_args ->
    let thunk = k main in
    fun `Run_main -> thunk ()
  in
  let flags = Flag.Internal.create flags in
  let base = { Base.summary; readme; usage; flags; anons } in
  let base =
    Bailout_dump_flag.add
      base
      ~name:"-help"
      ~aliases:[ "-?" ]
      ~aliases_excluded_from_help:[ "--help" ]
      ~text_summary:"this help text"
      ~text:(fun env -> Lazy.force (Env.find_exn env Base.help_key))
  in
  Base base
;;

let basic = basic_spec
let subs_key : (string * t) list Env.Key.t = Env.key_create "subcommands"

let lazy_group ~summary ?readme ?preserve_subcommand_order ?body alist =
  let subcommands =
    Lazy.map alist ~f:(fun alist ->
      let alist =
        List.map alist ~f:(fun (name, t) -> normalize Key_type.Subcommand name, t)
      in
      match String.Map.of_alist alist with
      | `Duplicate_key name -> failwithf "multiple subcommands named %s" name ()
      | `Ok map ->
        (match preserve_subcommand_order with
         | Some () -> alist
         | None -> Map.to_alist map))
  in
  Group { summary; readme; subcommands; body }
;;

let group ~summary ?readme ?preserve_subcommand_order ?body alist =
  lazy_group ~summary ?readme ?preserve_subcommand_order ?body (Lazy.from_val alist)
;;

let exec ~summary ?readme ?(child_subcommand = []) ~path_to_exe () =
  let working_dir =
    Filename.dirname
    @@
    match path_to_exe with
    | `Absolute _ | `Relative_to_me _ -> Sys.executable_name
    | `Relative_to_argv0 _ -> Caml.Sys.argv.(0)
  in
  let path_to_exe =
    match path_to_exe with
    | `Absolute p ->
      if not (Filename.is_absolute p)
      then failwith "Path passed to `Absolute must be absolute"
      else p
    | `Relative_to_me p | `Relative_to_argv0 p ->
      if not (Filename.is_relative p)
      then failwith "Path passed to `Relative_to_me must be relative"
      else p
  in
  Exec { summary; readme; working_dir; path_to_exe; child_subcommand }
;;

let of_lazy thunk = Lazy thunk

let rec proxy_of_sexpable
          sexpable
          ~working_dir
          ~path_to_exe
          ~child_subcommand
          ~path_to_subcommand
  : Proxy.t
  =
  let kind =
    kind_of_sexpable
      sexpable
      ~working_dir
      ~path_to_exe
      ~child_subcommand
      ~path_to_subcommand
  in
  { working_dir; path_to_exe; path_to_subcommand; child_subcommand; kind }

and kind_of_sexpable
      sexpable
      ~working_dir
      ~path_to_exe
      ~child_subcommand
      ~path_to_subcommand
  =
  match (sexpable : Shape.Sexpable.t) with
  | Base b -> Proxy.Kind.Base b
  | Exec e -> Proxy.Kind.Exec e
  | Lazy l ->
    Proxy.Kind.Lazy
      (Lazy.map l ~f:(fun sexpable ->
         kind_of_sexpable
           sexpable
           ~working_dir
           ~path_to_exe
           ~child_subcommand
           ~path_to_subcommand))
  | Group g ->
    Proxy.Kind.Group
      { g with
        subcommands =
          Lazy.map
            g.subcommands
            ~f:
              (List.map ~f:(fun (str, sexpable) ->
                 let path_to_subcommand = path_to_subcommand @ [ str ] in
                 let proxy =
                   proxy_of_sexpable
                     sexpable
                     ~working_dir
                     ~path_to_exe
                     ~child_subcommand
                     ~path_to_subcommand
                 in
                 str, proxy))
      }
;;

module Version_info = struct
  let sanitize_version ~version =
    (* [version] was space delimited at some point and newline delimited
       at another.  We always print one (repo, revision) pair per line
       and ensure sorted order *)
    String.split version ~on:' '
    |> List.concat_map ~f:(String.split ~on:'\n')
    |> List.sort ~compare:String.compare
  ;;

  let print_version ~version = List.iter (sanitize_version ~version) ~f:print_endline
  let print_build_info ~build_info = print_endline (force build_info)

  let command ~version ~build_info =
    basic
      ~summary:"print version information"
      Base.Spec.(
        empty
        +> flag "-version" no_arg ~doc:" print the version of this build"
        +> flag "-build-info" no_arg ~doc:" print build info for this build")
      (fun version_flag build_info_flag ->
         if build_info_flag
         then print_build_info ~build_info
         else if version_flag
         then print_version ~version
         else (
           print_build_info ~build_info;
           print_version ~version);
         exit 0)
  ;;

  let rec add ~version ~build_info unversioned =
    match unversioned with
    | Base base ->
      let base =
        Bailout_dump_flag.add
          base
          ~name:"-version"
          ~aliases:[]
          ~aliases_excluded_from_help:[ "--version" ]
          ~text_summary:"the version of this build"
          ~text:(fun _ -> String.concat ~sep:"\n" (sanitize_version ~version))
      in
      let base =
        Bailout_dump_flag.add
          base
          ~name:"-build-info"
          ~aliases:[]
          ~aliases_excluded_from_help:[ "--build-info" ]
          ~text_summary:"info about this build"
          ~text:(fun _ -> force build_info)
      in
      Base base
    | Group group ->
      let subcommands =
        Lazy.map group.Group.subcommands ~f:(fun subcommands ->
          extend_alist_exn
            subcommands
            Key_type.Subcommand
            ~key:"version"
            (command ~version ~build_info))
      in
      Group { group with Group.subcommands }
    | Proxy proxy -> Proxy proxy
    | Exec exec -> Exec exec
    | Lazy thunk -> Lazy (lazy (add ~version ~build_info (Lazy.force thunk)))
  ;;
end

let rec summary = function
  | Base x -> x.summary
  | Group x -> x.summary
  | Exec x -> x.summary
  | Proxy x -> Proxy.get_summary x
  | Lazy thunk -> summary (Lazy.force thunk)
;;

module Spec = struct
  include Base.Spec

  let path = map ~f:Path.parts_exe_basename path
end

module Deprecated = struct
  module Spec = Spec.Deprecated

  let summary = summary

  let rec get_flag_names = function
    | Base base -> base.Base.flags |> String.Map.keys
    | Lazy thunk -> get_flag_names (Lazy.force thunk)
    | Group _ | Proxy _ | Exec _ -> assert false
  ;;

  let help_recursive ~cmd ~with_flags ~expand_dots t s =
    let rec help_recursive_rec ~cmd t s =
      let new_s = s ^ (if expand_dots then cmd else ".") ^ " " in
      match t with
      | Lazy thunk ->
        let t = Lazy.force thunk in
        help_recursive_rec ~cmd t s
      | Base base ->
        let base_help = s ^ cmd, summary (Base base) in
        if with_flags
        then
          base_help
          :: List.map
               ~f:(fun (flag, h) -> new_s ^ flag, h)
               (List.sort
                  ~compare:Base.Deprecated.subcommand_cmp_fst
                  (Base.Deprecated.flags_help ~display_help_flags:false base))
        else [ base_help ]
      | Group { summary; subcommands; readme = _; body = _ } ->
        (s ^ cmd, summary)
        :: (Lazy.force subcommands
            |> List.sort ~compare:Base.Deprecated.subcommand_cmp_fst
            |> List.concat_map ~f:(fun (cmd', t) -> help_recursive_rec ~cmd:cmd' t new_s)
           )
      | Proxy _ | Exec _ ->
        (* Command.exec does not support deprecated commands *)
        []
    in
    help_recursive_rec ~cmd t s
  ;;
end

module For_unix (M : For_unix) = struct
  open M

  (* Clear the setting of environment variable associated with command-line
     completion and recursive help so that subprocesses don't see them.

     Use [unsafe_getenv] so setuid-root programs can still read environment variables.
     There is no security risk here because the values are only used as triggers to dump
     out command information. *)
  let getenv_and_clear var =
    let value = Unix.unsafe_getenv var in
    if Option.is_some value then Unix.unsetenv var;
    value
  ;;

  let maybe_comp_cword () = getenv_and_clear comp_cword |> Option.map ~f:Int.of_string

  let set_comp_cword new_value =
    let new_value = Int.to_string new_value in
    Unix.putenv ~key:comp_cword ~data:new_value
  ;;

  module Exec = struct
    include Exec

    let exec_with_args t ~args ~maybe_new_comp_cword =
      let prog = abs_path ~dir:t.working_dir t.path_to_exe in
      let args = t.child_subcommand @ args in
      Option.iter maybe_new_comp_cword ~f:(fun n ->
        (* The logic for tracking [maybe_new_comp_cword] doesn't take into account whether
           this exec specifies a child subcommand. If it does, COMP_CWORD needs to be set
           higher to account for the arguments used to specify the child subcommand. *)
        set_comp_cword (n + List.length t.child_subcommand));
      never_returns (Unix.exec ~prog ~argv:(prog :: args) ())
    ;;
  end

  module Sexpable = struct
    include Shape.Sexpable

    let read_stdout_and_stderr (process_info : Unix.Process_info.t) =
      (* We need to read each of stdout and stderr in a separate thread to avoid deadlocks
         if the child process decides to wait for a read on one before closing the other.
         Buffering may hide this problem until output is "sufficiently large". *)
      let start_reading descr info =
        let output = Set_once.create () in
        let thread =
          Thread.create
            ~on_uncaught_exn:`Print_to_stderr
            (fun () ->
               Result.try_with (fun () ->
                 descr |> Unix.in_channel_of_descr |> In_channel.input_all)
               |> Set_once.set_exn output [%here])
            ()
        in
        stage (fun () ->
          Thread.join thread;
          Unix.close descr;
          match Set_once.get output with
          | None -> raise_s [%message "BUG failed to read" (info : Info.t)]
          | Some (Ok output) -> output
          | Some (Error exn) -> raise exn)
      in
      (* We might hang forever trying to join the reading threads if the child process keeps
         the file descriptor open. Not handling this because I think we've never seen it
         in the wild despite running vulnerable code for years. *)
      (* We have to start both threads before joining any of them. *)
      let finish_stdout = start_reading process_info.stdout (Info.of_string "stdout") in
      let finish_stderr = start_reading process_info.stderr (Info.of_string "stderr") in
      unstage finish_stdout (), unstage finish_stderr ()
    ;;

    let of_external ~working_dir ~path_to_exe ~child_subcommand =
      let process_info =
        Unix.create_process_env
          ()
          ~prog:(abs_path ~dir:working_dir path_to_exe)
          ~args:child_subcommand
          ~env:
            (`Extend
               [ extraction_var, supported_versions |> Int.Set.sexp_of_t |> Sexp.to_string
               ])
      in
      Unix.close process_info.stdin;
      let stdout, stderr = read_stdout_and_stderr process_info in
      ignore (Unix.wait (`Pid process_info.pid) : Pid.t * Unix.Exit_or_signal.t);
      (* Now we've killed all the processes and threads we made. *)
      match stdout |> Sexp.of_string |> Versioned.t_of_sexp |> of_versioned with
      | exception exn ->
        raise_s
          [%message
            "cannot parse command shape"
              ~_:(exn : exn)
              (stdout : string)
              (stderr : string)]
      | t -> t
    ;;

    let rec find (t : t) ~path_to_subcommand =
      match path_to_subcommand with
      | [] -> t
      | sub :: subs ->
        (match t with
         | Base _ -> failwithf "unexpected subcommand %S" sub ()
         | Lazy thunk -> find (Lazy.force thunk) ~path_to_subcommand
         | Exec { path_to_exe; working_dir; child_subcommand; _ } ->
           find
             (of_external ~working_dir ~path_to_exe ~child_subcommand)
             ~path_to_subcommand:(sub :: (subs @ child_subcommand))
         | Group g ->
           (match List.Assoc.find (Lazy.force g.subcommands) ~equal:String.equal sub with
            | None -> failwithf "unknown subcommand %S" sub ()
            | Some t -> find t ~path_to_subcommand:subs))
    ;;
  end

  let proxy_of_exe ~working_dir path_to_exe child_subcommand =
    Sexpable.of_external ~working_dir ~path_to_exe ~child_subcommand
    |> proxy_of_sexpable
         ~working_dir
         ~path_to_exe
         ~child_subcommand
         ~path_to_subcommand:[]
  ;;

  let rec shape_of_proxy proxy : Shape.t = shape_of_proxy_kind proxy.Proxy.kind

  and shape_of_exe () ~child_subcommand ~path_to_exe ~working_dir =
    shape_of_proxy (proxy_of_exe ~working_dir path_to_exe child_subcommand)

  and shape_of_proxy_kind kind =
    match kind with
    | Base b -> Basic b
    | Lazy l -> Lazy (Lazy.map ~f:shape_of_proxy_kind l)
    | Group g ->
      Group
        { g with
          subcommands = Lazy.map g.subcommands ~f:(List.Assoc.map ~f:shape_of_proxy)
        }
    | Exec ({ child_subcommand; path_to_exe; working_dir; _ } as e) ->
      Exec (e, shape_of_exe ~child_subcommand ~path_to_exe ~working_dir)
  ;;

  let rec shape t : Shape.t =
    match t with
    | Base b -> Basic (Base.shape b)
    | Group g -> Group (Group.shape ~subcommand_to_shape:shape g)
    | Proxy p -> shape_of_proxy p
    | Exec ({ Exec.child_subcommand; path_to_exe; working_dir; _ } as e) ->
      Exec (Exec.shape e, shape_of_exe ~child_subcommand ~path_to_exe ~working_dir)
    | Lazy thunk -> shape (Lazy.force thunk)
  ;;

  let gather_help ~recursive ~flags ~expand_dots shape =
    let rec loop path acc shape =
      let string_of_path = if expand_dots then Path.to_string else Path.to_string_dots in
      let gather_group path acc subcommands =
        let filtered_subcommands =
          (* Only show the [help] subcommand at top-level. *)
          if Path.is_empty path
          then subcommands
          else List.Assoc.remove ~equal:String.( = ) subcommands "help"
        in
        filtered_subcommands
        |> List.stable_sort ~compare:(fun a b -> help_screen_compare (fst a) (fst b))
        |> List.fold
             ~init:acc
             ~f:(fun (acc : Shape.Flag_info.t Fqueue.t) (subcommand, shape) ->
               let path = Path.append path ~subcommand in
               let name = string_of_path path in
               let doc = Shape.get_summary shape in
               let acc = Fqueue.enqueue acc { name; doc; aliases = [] } in
               if recursive then loop path acc shape else acc)
      in
      match shape with
      | Exec (_, shape) ->
        (* If the executable being called doesn't use [Core.Command], then sexp extraction
           will fail. *)
        (try loop path acc (shape ()) with
         | _ -> acc)
      | Group g -> gather_group path acc (Lazy.force g.subcommands)
      | Basic b ->
        if flags
        then
          b.flags
          |> List.filter ~f:(fun fmt -> String.( <> ) fmt.name "[-help]")
          |> List.fold ~init:acc ~f:(fun acc fmt ->
            let path = Path.append path ~subcommand:fmt.name in
            let fmt = { fmt with name = string_of_path path } in
            Fqueue.enqueue acc fmt)
        else acc
      | Lazy thunk -> loop path acc (Lazy.force thunk)
    in
    loop Path.empty Fqueue.empty shape |> Fqueue.to_list
  ;;

  let group_or_exec_help_text ~flags ~path ~summary ~readme ~format_list =
    unparagraphs
      (List.filter_opt
         [ Some summary
         ; Some (String.concat [ "  "; Path.to_string path; " SUBCOMMAND" ])
         ; readme
         ; Some
             (if flags then "=== subcommands and flags ===" else "=== subcommands ===")
         ; Some (Shape.Flag_info.to_string format_list)
         ])
  ;;

  let rec help_for_shape shape path ~expand_dots ~flags ~recursive =
    let format_list = gather_help ~expand_dots ~flags ~recursive shape in
    match shape with
    | Basic b ->
      let usage = Shape.Base_info.get_usage b in
      unparagraphs
        (List.filter_opt
           [ Some b.summary
           ; Some ("  " ^ Path.to_string path ^ " " ^ usage)
           ; b.readme
           ; Some "=== flags ==="
           ; Some (Shape.Flag_info.to_string b.flags)
           ])
    | Group g ->
      group_or_exec_help_text
        ~flags
        ~path
        ~readme:g.readme
        ~summary:g.summary
        ~format_list
    | Exec (e, _) ->
      group_or_exec_help_text
        ~flags
        ~path
        ~readme:e.readme
        ~summary:e.summary
        ~format_list
    | Lazy thunk -> help_for_shape (Lazy.force thunk) path ~expand_dots ~flags ~recursive
  ;;

  let help_subcommand ~summary ~readme =
    basic
      ~summary:"explain a given subcommand (perhaps recursively)"
      Base.Spec.(
        empty
        +> flag "-recursive" no_arg ~doc:" show subcommands of subcommands, etc."
        +> flag "-flags" no_arg ~doc:" show flags as well in recursive help"
        +> flag "-expand-dots" no_arg ~doc:" expand subcommands in recursive help"
        +> path
        +> env
        +> anon (maybe ("SUBCOMMAND" %: string)))
      (fun recursive flags expand_dots path (env : Env.t) cmd_opt () ->
         let subs =
           match Env.find env subs_key with
           | Some subs -> subs
           | None -> assert false
           (* maintained by [dispatch] *)
         in
         let path =
           let path = Path.pop_help path in
           Option.fold cmd_opt ~init:path ~f:(fun path subcommand ->
             Path.append path ~subcommand)
         in
         let path, shape =
           match cmd_opt with
           | None ->
             let subcommands = List.Assoc.map subs ~f:shape |> Lazy.from_val in
             let readme = Option.map readme ~f:(fun readme -> readme ()) in
             path, Shape.Group { readme; summary; subcommands }
           | Some cmd ->
             (match
                lookup_expand
                  (List.Assoc.map subs ~f:(fun x -> x, `Prefix))
                  cmd
                  Subcommand
              with
              | Error e ->
                die
                  "unknown subcommand %s for command %s: %s"
                  cmd
                  (Path.to_string path)
                  e
                  ()
              | Ok (possibly_expanded_name, t) ->
                (* Fix the unexpanded value *)
                let path = Path.replace_first ~from:cmd ~to_:possibly_expanded_name path in
                path, shape t)
         in
         print_endline (help_for_shape shape path ~recursive ~flags ~expand_dots))
  ;;

  (* This script works in both bash (via readarray) and zsh (via read -A).  If you change
     it, please test in both bash and zsh.  It does not work tcsh (different function
     syntax). *)
  let dump_autocomplete_function () =
    let fname = sprintf "_jsautocom_%s" (Pid.to_string (Unix.getpid ())) in
    let argv_0 = Caml.Sys.argv.(0) in
    printf
      "function %s {\n\
      \  export COMP_CWORD\n\
      \  COMP_WORDS[0]=%s\n\
      \  if type readarray > /dev/null\n\
      \  then readarray -t COMPREPLY < <(\"${COMP_WORDS[@]}\")\n\
      \  else IFS=\"\n\
       \" read -d \"\" -A COMPREPLY < <(\"${COMP_WORDS[@]}\")\n\
      \  fi\n\
       }\n\
       complete -F %s %s\n\
       %!"
      fname
      argv_0
      fname
      argv_0
  ;;

  let dump_help_sexp ~supported_versions t ~path_to_subcommand =
    Int.Set.inter Sexpable.supported_versions supported_versions
    |> Int.Set.max_elt
    |> function
    | None ->
      failwiths
        ~here:[%here]
        "Couldn't choose a supported help output version for Command.exec from the \
         given supported versions."
        Sexpable.supported_versions
        Int.Set.sexp_of_t
    | Some version_to_use ->
      sexpable_shape t
      |> Sexpable.find ~path_to_subcommand
      |> Sexpable.to_versioned ~version_to_use
      |> Sexpable.Versioned.sexp_of_t
      |> Sexp.to_string
      |> print_string
  ;;

  let handle_environment t ~argv =
    match argv with
    | [] -> failwith "missing executable name"
    | cmd :: args ->
      Option.iter (getenv_and_clear Sexpable.extraction_var) ~f:(fun version ->
        let supported_versions = Sexp.of_string version |> Int.Set.t_of_sexp in
        dump_help_sexp ~supported_versions t ~path_to_subcommand:args;
        exit 0);
      Option.iter (getenv_and_clear "COMMAND_OUTPUT_INSTALLATION_BASH") ~f:(fun _ ->
        dump_autocomplete_function ();
        exit 0);
      cmd, args
  ;;

  let process_args ~cmd ~args =
    let maybe_comp_cword = maybe_comp_cword () in
    let args =
      match maybe_comp_cword with
      | None -> Cmdline.of_list args
      | Some comp_cword ->
        let args = List.take (args @ [ "" ]) comp_cword in
        List.fold_right args ~init:Cmdline.Nil ~f:(fun arg args ->
          match args with
          | Cmdline.Nil -> Cmdline.Complete arg
          | _ -> Cmdline.Cons (arg, args))
    in
    Path.create ~path_to_exe:cmd, args, maybe_comp_cword
  ;;

  let rec add_help_subcommands = function
    | Base _ as t -> t
    | Exec _ as t -> t
    | Proxy _ as t -> t
    | Group { summary; readme; subcommands; body } ->
      let subcommands =
        Lazy.map subcommands ~f:(fun subcommands ->
          extend_alist_exn
            (List.Assoc.map subcommands ~f:add_help_subcommands)
            Key_type.Subcommand
            ~key:"help"
            (help_subcommand ~summary ~readme))
      in
      Group { summary; readme; subcommands; body }
    | Lazy thunk -> Lazy (lazy (add_help_subcommands (Lazy.force thunk)))
  ;;

  let maybe_apply_extend args ~extend ~path =
    Option.value_map extend ~default:args ~f:(fun f ->
      Cmdline.extend args ~extend:f ~path)
  ;;

  let rec dispatch
            t
            env
            ~extend
            ~path
            ~args
            ~maybe_new_comp_cword
            ~version
            ~build_info
            ~verbose_on_parse_error
            ~when_parsing_succeeds
    =
    match t with
    | Lazy thunk ->
      let t = Lazy.force thunk in
      dispatch
        t
        env
        ~extend
        ~path
        ~args
        ~maybe_new_comp_cword
        ~version
        ~build_info
        ~verbose_on_parse_error
        ~when_parsing_succeeds
    | Base base ->
      let args = maybe_apply_extend args ~extend ~path in
      let help_text =
        lazy
          (help_for_shape (shape t) path ~recursive:false ~flags:true ~expand_dots:false)
      in
      Base.run
        base
        env
        ~path
        ~args
        ~verbose_on_parse_error
        ~help_text
        ~when_parsing_succeeds
    | Exec exec ->
      let args = Cmdline.to_list (maybe_apply_extend args ~extend ~path) in
      Exec.exec_with_args ~args exec ~maybe_new_comp_cword
    | Proxy proxy ->
      let args =
        proxy.path_to_subcommand
        @ Cmdline.to_list (maybe_apply_extend args ~extend ~path)
      in
      let exec =
        { Exec.working_dir = proxy.working_dir
        ; path_to_exe = proxy.path_to_exe
        ; child_subcommand = proxy.child_subcommand
        ; summary = Proxy.get_summary proxy
        ; readme = Proxy.get_readme proxy |> Option.map ~f:const
        }
      in
      Exec.exec_with_args ~args exec ~maybe_new_comp_cword
    | Group ({ summary; readme; subcommands = subs; body } as group) ->
      let env = Env.set env subs_key (Lazy.force subs) in
      let die_showing_help msg =
        if not (Cmdline.ends_in_complete args)
        then (
          eprintf
            "%s\n%!"
            (help_for_shape
               ~recursive:false
               ~flags:false
               ~expand_dots:false
               (shape (Group { summary; readme; subcommands = subs; body }))
               path);
          die "%s" msg ())
      in
      (match args with
       | Nil ->
         (match body with
          | None ->
            die_showing_help
              (sprintf "missing subcommand for command %s" (Path.to_string path))
          | Some body -> body ~path:(Path.parts_exe_basename path))
       | Cons (sub, rest) ->
         let sub, rest =
           (* Match for flags recognized when subcommands are expected next *)
           match sub, rest with
           (* Recognized at the top level command only *)
           | ("-version" | "--version"), _ when Path.length path = 1 ->
             Version_info.print_version ~version;
             exit 0
           | ("-build-info" | "--build-info"), _ when Path.length path = 1 ->
             Version_info.print_build_info ~build_info;
             exit 0
           (* Recognized everywhere *)
           | ("-help" | "--help"), Nil ->
             print_endline
               (help_for_shape
                  ~recursive:false
                  ~flags:false
                  ~expand_dots:false
                  (shape (Group { group with subcommands = subs }))
                  path);
             exit 0
           | ("-help" | "--help"), Cmdline.Cons (sub, rest) ->
             sub, Cmdline.Cons ("-help", rest)
           | _ -> sub, rest
         in
         (match
            lookup_expand
              (List.Assoc.map (Lazy.force subs) ~f:(fun x -> x, `Prefix))
              sub
              Subcommand
          with
          | Error msg -> die_showing_help msg
          | Ok (sub, t) ->
            dispatch
              t
              env
              ~when_parsing_succeeds
              ~extend
              ~path:(Path.append path ~subcommand:sub)
              ~args:rest
              ~maybe_new_comp_cword:(Option.map ~f:Int.pred maybe_new_comp_cword)
              ~version
              ~build_info
              ~verbose_on_parse_error)
       | Complete part ->
         let subs =
           Lazy.force subs
           |> List.map ~f:fst
           |> List.filter ~f:(fun name -> String.is_prefix name ~prefix:part)
           |> List.sort ~compare:String.compare
         in
         List.iter subs ~f:print_endline;
         exit 0)
  ;;

  let default_version, default_build_info =
    ( Version_util.version
      , (* lazy to avoid loading all the time zone stuff at toplevel *)
      lazy (Version_util.reprint_build_info Time.sexp_of_t) )
  ;;

  let run
        ?verbose_on_parse_error
        ?(version = default_version)
        ?build_info
        ?(argv = Array.to_list Caml.Sys.argv)
        ?extend
        ?(when_parsing_succeeds = Fn.id)
        t
    =
    let build_info =
      match build_info with
      | Some v -> lazy v
      | None -> default_build_info
    in
    Exn.handle_uncaught_and_exit (fun () ->
      let t = Version_info.add t ~version ~build_info in
      let t = add_help_subcommands t in
      let cmd, args = handle_environment t ~argv in
      let path, args, maybe_new_comp_cword = process_args ~cmd ~args in
      try
        dispatch
          t
          Env.empty
          ~extend
          ~path
          ~args
          ~maybe_new_comp_cword
          ~version
          ~build_info
          ~verbose_on_parse_error
          ~when_parsing_succeeds
      with
      | Failed_to_parse_command_line msg ->
        if Cmdline.ends_in_complete args
        then exit 0
        else (
          prerr_endline msg;
          exit 1))
  ;;

  let deprecated_run
        t
        ~cmd
        ~args
        ~is_help
        ~is_help_rec
        ~is_help_rec_flags
        ~is_expand_dots
    =
    let path_strings = String.split cmd ~on:' ' in
    let path = Path.of_parts path_strings in
    let args = if is_expand_dots then "-expand-dots" :: args else args in
    let args = if is_help_rec_flags then "-flags" :: args else args in
    let args = if is_help_rec then "-r" :: args else args in
    let args = if is_help then "-help" :: args else args in
    let args = Cmdline.of_list args in
    let t = add_help_subcommands t in
    dispatch
      t
      Env.empty
      ~path
      ~args
      ~extend:None
      ~maybe_new_comp_cword:None
      ~version:default_version
      ~build_info:default_build_info
      ~verbose_on_parse_error:None
      ~when_parsing_succeeds:Fn.id
  ;;
end

(* NOTE: all that follows is simply namespace management boilerplate.  This will go away
   once we re-work the internals of Command to use Applicative from the ground up. *)

module Param = struct
  module type S = sig
    type +'a t

    include Applicative.S with type 'a t := 'a t

    val help : string Lazy.t t
    val path : string list t
    val args : string list t

    val flag
      :  ?aliases:string list
      -> ?full_flag_required:unit
      -> string
      -> 'a Flag.t
      -> doc:string
      -> 'a t

    val flag_optional_with_default_doc
      :  ?aliases:string list
      -> ?full_flag_required:unit
      -> string
      -> 'a Arg_type.t
      -> ('a -> Sexp.t)
      -> default:'a
      -> doc:string
      -> 'a t

    val anon : 'a Anons.t -> 'a t

    module If_nothing_chosen : sig
      type (_, _) t =
        | Default_to : 'a -> ('a, 'a) t
        | Raise : ('a, 'a) t
        | Return_none : ('a, 'a option) t
    end

    val choose_one
      :  'a option t list
      -> if_nothing_chosen:('a, 'b) If_nothing_chosen.t
      -> 'b t

    val and_arg_names : 'a t -> ('a * string list) t
    val and_arg_name : 'a t -> ('a * string) t
  end

  module A = struct
    type 'a t = 'a Spec.param

    include Applicative.Make (struct
        type nonrec 'a t = 'a t

        let return = Spec.const
        let apply = Spec.apply
        let map = `Custom Spec.map
      end)
  end

  include A

  let help = Spec.help
  let path = Spec.path
  let args = Spec.args
  let flag = Spec.flag
  let anon = Spec.anon
  let choose_one = Spec.choose_one
  let and_arg_names = Spec.and_arg_names
  let and_arg_name = Spec.and_arg_name
  let flag_optional_with_default_doc = Spec.flag_optional_with_default_doc

  module Arg_type = Arg_type
  module If_nothing_chosen = Spec.If_nothing_chosen
  include Arg_type.Export

  include struct
    open Flag

    let escape = escape
    let listed = listed
    let map_flag = map_flag
    let no_arg = no_arg
    let no_arg_abort = no_arg_abort
    let no_arg_register = no_arg_register
    let no_arg_some = no_arg_some
    let one_or_more = one_or_more
    let optional = optional
    let optional_with_default = optional_with_default
    let required = required
  end

  include struct
    open Anons

    let ( %: ) = ( %: )
    let map_anons = map_anons
    let maybe = maybe
    let maybe_with_default = maybe_with_default
    let non_empty_sequence_as_list = non_empty_sequence_as_list
    let non_empty_sequence_as_pair = non_empty_sequence_as_pair
    let sequence = sequence
    let t2 = t2
    let t3 = t3
    let t4 = t4
  end
end

module Let_syntax = struct
  include Param

  module Let_syntax = struct
    include Param
    module Open_on_rhs = Param
  end
end

type 'result basic_command =
  summary:string -> ?readme:(unit -> string) -> (unit -> 'result) Param.t -> t

let basic ~summary ?readme param =
  let spec = Spec.of_params @@ Param.map param ~f:(fun run () () -> run ()) in
  let readme = Option.map readme ~f:(fun f () -> String.strip (f ())) in
  basic ~summary ?readme spec ()
;;

module Private = struct
  let abs_path = abs_path
  let word_wrap = Shape.Private.word_wrap

  module Anons = Anons
  module Cmdline = Cmdline
  module For_unix = For_unix
  module Path = Path

  module Spec = struct
    include Spec

    let to_string_for_choose_one param =
      Choose_one.Choice_name.(create_exn param |> to_string)
    ;;
  end
end
