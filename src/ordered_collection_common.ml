open Result.Export
module List = ListLabels

let invalid_argf = Core_printf.invalid_argf

let normalize ~length_fun t i =
  if i < 0
  then i + length_fun t
  else i

let slice ~length_fun ~sub_fun t start stop =
  let stop = if stop = 0 then length_fun t else stop in
  let pos = normalize ~length_fun t start in
  let len = (normalize ~length_fun t stop) - pos in
  sub_fun t ~pos ~len

let check_pos_len_exn ~pos ~len ~length =
  if pos < 0
  then invalid_argf "Negative position: %d" pos ();
  if len < 0
  then invalid_argf "Negative length: %d" len ();
  if pos > length - len
  then invalid_argf "pos + len past end: %d + %d > %d" pos len length ()
;;

let%test_unit _ =
  let vals = [ -1; 0; 1; 2; 3 ] in
  List.iter [ 0; 1; 2 ] ~f:(fun length ->
    List.iter vals ~f:(fun pos ->
      List.iter vals ~f:(fun len ->
        let result = Result.try_with (fun () -> check_pos_len_exn ~pos ~len ~length) in
        let valid = pos >= 0 && len >= 0 && len <= length - pos in
        assert (valid = Result.is_ok result))))
;;

let get_pos_len_exn ?(pos = 0) ?len ~length =
  let len = match len with Some i -> i | None -> length - pos in
  check_pos_len_exn ~pos ~len ~length;
  pos, len
;;

let%test_unit _ =
  let opts = [ None; Some (-1); Some 0; Some 1; Some 2 ] in
  List.iter [ 0; 1; 2 ] ~f:(fun length ->
    List.iter opts ~f:(fun pos ->
      List.iter opts ~f:(fun len ->
        let result = Result.try_with (fun () -> get_pos_len_exn ?pos ?len ~length) in
        let pos = match pos with Some x -> x | None -> 0 in
        let len = match len with Some x -> x | None -> length - pos in
        let valid = pos >= 0 && len >= 0 && len <= length - pos in
        match result with
        | Error _ -> assert (not valid);
        | Ok (pos', len') ->
          assert (pos' = pos);
          assert (len' = len);
          assert valid)))
;;

let get_pos_len ?pos ?len ~length =
  try Result.Ok (get_pos_len_exn ?pos ?len ~length)
  with Invalid_argument s -> Result.Error s
