module Stable = struct
  open! Stable_internal
  open! Ppx_compare_lib.Builtin

  module Anons = struct
    module Grammar = struct
      module V1 = struct
        type t =
          | Zero
          | One of string
          | Many of t
          | Maybe of t
          | Concat of t list
          | Ad_hoc of string
        [@@deriving bin_io, compare, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| a17fd34ec213e508db450f6469f7fe99 |}]
        ;;

        let rec invariant t =
          Base.Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
            match t with
            | Zero -> ()
            | One _ -> ()
            | Many Zero -> failwith "Many Zero should be just Zero"
            | Many t -> invariant t
            | Maybe Zero -> failwith "Maybe Zero should be just Zero"
            | Maybe t -> invariant t
            | Concat [] | Concat [ _ ] ->
              failwith "Flatten zero and one-element Concat"
            | Concat ts -> Base.List.iter ts ~f:invariant
            | Ad_hoc _ -> ())
        ;;

        let t_of_sexp sexp =
          let t = [%of_sexp: t] sexp in
          invariant t;
          t
        ;;

        let rec usage = function
          | Zero -> ""
          | One usage -> usage
          | Many Zero -> failwith "bug in command.ml"
          | Many (One _ as t) -> Base.Printf.sprintf "[%s ...]" (usage t)
          | Many t -> Base.Printf.sprintf "[(%s) ...]" (usage t)
          | Maybe Zero -> failwith "bug in command.ml"
          | Maybe t -> Base.Printf.sprintf "[%s]" (usage t)
          | Concat ts -> Base.String.concat ~sep:" " (Base.List.map ts ~f:usage)
          | Ad_hoc usage -> usage
        ;;
      end

      module Model = V1
    end

    module V2 = struct
      type t =
        | Usage of string
        | Grammar of Grammar.V1.t
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 081d9ec167903f8f8c49cbf8e3fb3a66 |}]
      ;;
    end

    module Model = V2
  end

  module Flag_info = struct
    module V1 = struct
      type t =
        { name : string
        ; doc : string
        ; aliases : string list
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bd8d6fb7a662d2c0b5e0d2026c6d2d21 |}]
      ;;
    end

    module Model = V1
  end

  module Base_info = struct
    module V2 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; anons : Anons.V2.t
        ; flags : Flag_info.V1.t list
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8faac1e8d9deb0baaa56ac8ebf85b498 |}]
      ;;
    end

    module V1 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; usage : string
        ; flags : Flag_info.V1.t list
        }
      [@@deriving bin_shape, sexp]

      let to_latest { summary; readme; usage; flags } =
        { V2.summary; readme; anons = Usage usage; flags }
      ;;

      let of_latest { V2.summary; readme; anons; flags } =
        { summary
        ; readme
        ; usage =
            (match anons with
             | Usage usage -> usage
             | Grammar grammar -> Anons.Grammar.V1.usage grammar)
        ; flags
        }
      ;;
    end

    module Model = V2
  end

  module Group_info = struct
    type a = Dummy_type_because_we_cannot_digest_type_constructors_only_concrete_types
    [@@deriving bin_io]

    module V2 = struct
      type 'a t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; subcommands : (string * 'a) List.Stable.V1.t Lazy.Stable.V1.t
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: a t];
        [%expect {| 2cc3eeb58d12d8fe4400009e592d7827 |}]
      ;;

      open! Base

      let map t ~f =
        { t with subcommands = Lazy.map t.subcommands ~f:(List.Assoc.map ~f) }
      ;;
    end

    module Model = V2

    module V1 = struct
      type 'a t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; subcommands : (string * 'a) List.Stable.V1.t
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: a t];
        [%expect {| 2cc3eeb58d12d8fe4400009e592d7827 |}]
      ;;

      open! Base

      let map t ~f = { t with subcommands = List.Assoc.map t.subcommands ~f }

      let to_latest { summary; readme; subcommands } : 'a Model.t =
        { summary; readme; subcommands = Lazy.from_val subcommands }
      ;;

      let of_latest ({ summary; readme; subcommands } : 'a Model.t) : 'a t =
        { summary; readme; subcommands = Lazy.force subcommands }
      ;;
    end
  end

  module Exec_info = struct
    let abs_path ~dir path =
      if Filename.is_absolute path then path else Filename.concat dir path
    ;;

    module V3 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; working_dir : string
        ; path_to_exe : string
        ; child_subcommand : string list
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| c0c8256e9238cdd8f2ec1f8785e02ae0 |}]
      ;;

      let to_latest = Fn.id
      let of_latest = Fn.id
    end

    module Model = V3

    module V2 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; working_dir : string
        ; path_to_exe : string
        }
      [@@deriving bin_shape, sexp]

      let to_v3 t : V3.t =
        { summary = t.summary
        ; readme = t.readme
        ; working_dir = t.working_dir
        ; path_to_exe = t.path_to_exe
        ; child_subcommand = []
        }
      ;;

      let of_v3 (t : V3.t) =
        { summary = t.summary
        ; readme = t.readme
        ; working_dir = t.working_dir
        ; path_to_exe = abs_path ~dir:t.working_dir t.path_to_exe
        }
      ;;

      let to_latest = Fn.compose V3.to_latest to_v3
      let of_latest = Fn.compose of_v3 V3.of_latest
    end

    module V1 = struct
      type t =
        { summary : string
        ; readme : string option [@sexp.option]
        ; (* [path_to_exe] must be absolute. *)
          path_to_exe : string
        }
      [@@deriving bin_shape, sexp]

      let to_v2 t : V2.t =
        { summary = t.summary
        ; readme = t.readme
        ; working_dir = "/"
        ; path_to_exe = t.path_to_exe
        }
      ;;

      let of_v2 (t : V2.t) =
        { summary = t.summary
        ; readme = t.readme
        ; path_to_exe = abs_path ~dir:t.working_dir t.path_to_exe
        }
      ;;

      let to_latest = Fn.compose V2.to_latest to_v2
      let of_latest = Fn.compose of_v2 V2.of_latest
    end
  end

  module Fully_forced = struct
    module V1 = struct
      type t =
        | Basic of Base_info.V2.t
        | Group of t Group_info.V2.t
        | Exec of Exec_info.V3.t * t
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 981154ef3919437c6c822619882841d4 |}]
      ;;
    end

    module Model = V1
  end

  module Sexpable = struct
    module V3 = struct
      type t =
        | Base of Base_info.V2.t
        | Group of t Group_info.V2.t
        | Exec of Exec_info.V3.t
        | Lazy of t Lazy.Stable.V1.t
      [@@deriving bin_shape, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d3c375548f1a43c58c71e814c04ba36a |}]
      ;;

      let to_latest = Fn.id
      let of_latest = Fn.id
    end

    module Model = V3

    module V2 = struct
      type t =
        | Base of Base_info.V2.t
        | Group of t Group_info.V1.t
        | Exec of Exec_info.V2.t
      [@@deriving bin_shape, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 598d4b41ef435bc69a4886bdba0f8689 |}]
      ;;

      let rec to_latest : t -> Model.t = function
        | Base b -> Base b
        | Exec e -> Exec (Exec_info.V2.to_latest e)
        | Group g -> Group (Group_info.V1.to_latest (Group_info.V1.map g ~f:to_latest))
      ;;

      let rec of_latest : Model.t -> t = function
        | Base b -> Base b
        | Exec e -> Exec (Exec_info.V2.of_latest e)
        | Lazy thunk -> of_latest (Base.Lazy.force thunk)
        | Group g -> Group (Group_info.V1.map (Group_info.V1.of_latest g) ~f:of_latest)
      ;;
    end

    module V1 = struct
      type t =
        | Base of Base_info.V1.t
        | Group of t Group_info.V1.t
        | Exec of Exec_info.V1.t
      [@@deriving bin_shape, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 70d20b5432ffab77a385b02b04031d2e |}]
      ;;

      let rec to_latest : t -> Model.t = function
        | Base b -> Base (Base_info.V1.to_latest b)
        | Exec e -> Exec (Exec_info.V1.to_latest e)
        | Group g -> Group (Group_info.V1.to_latest (Group_info.V1.map g ~f:to_latest))
      ;;

      let rec of_latest : Model.t -> t = function
        | Base b -> Base (Base_info.V1.of_latest b)
        | Exec e -> Exec (Exec_info.V1.of_latest e)
        | Lazy thunk -> of_latest (Base.Lazy.force thunk)
        | Group g -> Group (Group_info.V1.map (Group_info.V1.of_latest g) ~f:of_latest)
      ;;
    end

    module Versioned = struct
      type t =
        | V1 of V1.t
        | V2 of V2.t
        | V3 of V3.t (* available at least since 2020-04 *)
      [@@deriving bin_shape, sexp, variants]

      (* It's okay to change this one in place, as long as we wait long enough before
         dropping support for old versions. *)
      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 796fbf006deb25159171637c2a510bf4 |}]
      ;;

      let to_latest = function
        | V1 t -> V1.to_latest t
        | V2 t -> V2.to_latest t
        | V3 t -> V3.to_latest t
      ;;

      let of_latest ~version_to_use latest =
        match version_to_use with
        | 1 -> V1 (V1.of_latest latest)
        | 2 -> V2 (V2.of_latest latest)
        | 3 -> V3 (V3.of_latest latest)
        | other ->
          Std_internal.failwiths
            ~here:[%here]
            "unsupported version_to_use"
            other
            [%sexp_of: int]
      ;;
    end
  end
end

open! Import
open! Std_internal

module Anons = struct
  module Grammar = struct
    type t = Stable.Anons.Grammar.Model.t =
      | Zero
      | One of string
      | Many of t
      | Maybe of t
      | Concat of t list
      | Ad_hoc of string
    [@@deriving bin_io, compare, sexp]

    let invariant = Stable.Anons.Grammar.Model.invariant
    let usage = Stable.Anons.Grammar.Model.usage
  end

  type t = Stable.Anons.Model.t =
    | Usage of string
    | Grammar of Grammar.t
  [@@deriving bin_io, compare, sexp]
end

module Flag_info = struct
  type t = Stable.Flag_info.Model.t =
    { name : string
    ; doc : string
    ; aliases : string list
    }
  [@@deriving bin_io, compare, fields, sexp]

  let help_screen_compare a b =
    match a, b with
    | _, "[-help]" -> -1
    | "[-help]", _ -> 1
    | _, "[-version]" -> -1
    | "[-version]", _ -> 1
    | _, "[-build-info]" -> -1
    | "[-build-info]", _ -> 1
    | _, "help" -> -1
    | "help", _ -> 1
    | _, "version" -> -1
    | "version", _ -> 1
    | _ -> 0
  ;;

  let sort ts =
    List.stable_sort ts ~compare:(fun a b -> help_screen_compare a.name b.name)
  ;;

  let word_wrap text width =
    let chunks = String.split text ~on:'\n' in
    List.concat_map chunks ~f:(fun text ->
      let words =
        String.split text ~on:' '
        |> List.filter ~f:(fun word -> not (String.is_empty word))
      in
      match
        List.fold words ~init:None ~f:(fun acc word ->
          Some
            (match acc with
             | None -> [], word
             | Some (lines, line) ->
               (* efficiency is not a concern for the string lengths we expect *)
               let line_and_word = line ^ " " ^ word in
               if String.length line_and_word <= width
               then lines, line_and_word
               else line :: lines, word))
      with
      | None -> []
      | Some (lines, line) -> List.rev (line :: lines))
  ;;

  let to_string ts =
    let n = List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (String.length t.name)) in
    let num_cols = 80 in
    (* anything more dynamic is likely too brittle *)
    let extend x =
      let slack = n - String.length x in
      x ^ String.make slack ' '
    in
    let lhs_width = n + 4 in
    let lhs_pad = String.make lhs_width ' ' in
    String.concat
      (List.map ts ~f:(fun t ->
         let rows k v =
           let vs = word_wrap v (num_cols - lhs_width) in
           match vs with
           | [] -> [ "  "; k; "\n" ]
           | v :: vs ->
             let first_line = [ "  "; extend k; "  "; v; "\n" ] in
             let rest_lines = List.map vs ~f:(fun v -> [ lhs_pad; v; "\n" ]) in
             List.concat (first_line :: rest_lines)
         in
         String.concat
           (List.concat
              (rows t.name t.doc
               ::
               (match t.aliases with
                | [] -> []
                | [ x ] -> [ rows "" (sprintf "(alias: %s)" x) ]
                | xs -> [ rows "" (sprintf "(aliases: %s)" (String.concat ~sep:", " xs)) ])
              ))))
  ;;
end

module Base_info = struct
  type t = Stable.Base_info.Model.t =
    { summary : string
    ; readme : string option [@sexp.option]
    ; anons : Anons.t
    ; flags : Flag_info.t list
    }
  [@@deriving bin_io, compare, fields, sexp]

  let get_usage t =
    match t.anons with
    | Usage usage -> usage
    | Grammar grammar -> Stable.Anons.Grammar.V1.usage grammar
  ;;
end

module Group_info = struct
  type 'a t = 'a Stable.Group_info.Model.t =
    { summary : string
    ; readme : string option [@sexp.option]
    ; subcommands : (string * 'a) List.t Lazy.t
    }
  [@@deriving bin_io, compare, fields, sexp]

  let map = Stable.Group_info.Model.map
end

module Exec_info = struct
  type t = Stable.Exec_info.Model.t =
    { summary : string
    ; readme : string option [@sexp.option]
    ; working_dir : string
    ; path_to_exe : string
    ; child_subcommand : string list
    }
  [@@deriving bin_io, compare, fields, sexp]
end

module Fully_forced = struct
  type t = Stable.Fully_forced.Model.t =
    | Basic of Base_info.t
    | Group of t Group_info.t
    | Exec of Exec_info.t * t
  [@@deriving bin_io, compare, sexp]
end

module Sexpable = struct
  type t = Stable.Sexpable.Model.t =
    | Base of Base_info.t
    | Group of t Group_info.t
    | Exec of Exec_info.t
    | Lazy of t Lazy.t
  [@@deriving sexp_of]

  let extraction_var = "COMMAND_OUTPUT_HELP_SEXP"

  module Versioned = Stable.Sexpable.Versioned

  let supported_versions =
    let f i supported _ = Set.add supported i in
    Versioned.Variants.fold ~init:(Set.empty (module Int)) ~v1:(f 1) ~v2:(f 2) ~v3:(f 3)
  ;;

  let of_versioned = Versioned.to_latest
  let to_versioned t ~version_to_use = Versioned.of_latest t ~version_to_use
end

type t =
  | Basic of Base_info.t
  | Group of t Group_info.t
  | Exec of Exec_info.t * (unit -> t)
  | Lazy of t Lazy.t

let rec fully_forced : t -> Fully_forced.t = function
  | Basic b -> Basic b
  | Group g -> Group (Group_info.map g ~f:fully_forced)
  | Exec (e, f) -> Exec (e, fully_forced (f ()))
  | Lazy thunk -> fully_forced (Lazy.force thunk)
;;

let rec get_summary = function
  | Basic b -> b.summary
  | Group g -> g.summary
  | Exec (e, _) -> e.summary
  | Lazy thunk -> get_summary (Lazy.force thunk)
;;

module Private = struct
  let abs_path = Stable.Exec_info.abs_path
  let help_screen_compare = Flag_info.help_screen_compare
  let word_wrap = Flag_info.word_wrap
end
