open! Core

module Make (M : sig
  val name : string
  val start_marker : string
  val length_including_start_marker : int
end) =
struct
  let chop_start_marker_if_exists = String.chop_prefix_if_exists ~prefix:M.start_marker

  let get ~contents_of_exe =
    let%map.Option i = String.substr_index contents_of_exe ~pattern:M.start_marker in
    String.slice
      contents_of_exe
      (i + String.length M.start_marker)
      (i + M.length_including_start_marker)
    |> String.take_while ~f:(Char.( <> ) '\000')
  ;;

  let pad str n = str ^ String.make (n - String.length str) '\000'

  let pad_with_at_least_one_nul_byte_exn string =
    if String.mem string '\000' then failwith "sections can't contain NUL bytes";
    let len = M.length_including_start_marker - String.length M.start_marker in
    if len > String.length string
    then (* This ensures we add at least one NUL byte *)
      pad string len
    else failwithf "%s must be shorter than %d bytes" M.name len ()
  ;;

  let replace ~contents_of_exe ~data =
    let raw_data = pad_with_at_least_one_nul_byte_exn data in
    (* There can be two places to rewrite, because apparently in the presence of weakdefs,
       both defs end up in the exe. *)
    match
      String.substr_index_all contents_of_exe ~may_overlap:false ~pattern:M.start_marker
    with
    | [] -> None
    | _ :: _ as l ->
      let b = Bytes.of_string contents_of_exe in
      List.iter l ~f:(fun i ->
        let start = i + String.length M.start_marker in
        Stdlib.StdLabels.Bytes.blit_string
          ~src:raw_data
          ~src_pos:0
          ~dst:b
          ~dst_pos:start
          ~len:(String.length raw_data));
      Some (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b)
  ;;

  let count_occurrences ~contents_of_exe =
    List.length
      (String.substr_index_all contents_of_exe ~may_overlap:false ~pattern:M.start_marker)
  ;;

  module Expert = struct
    let start_marker = M.start_marker
    let pad_with_at_least_one_nul_byte_exn = pad_with_at_least_one_nul_byte_exn
  end
end
