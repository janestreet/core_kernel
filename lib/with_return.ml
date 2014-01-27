(* belongs in Common, but moved here to avoid circular dependencies *)

type 'a return = { return : 'b. 'a -> 'b }

let with_return (type a) f =
  let module M = struct
    (* Raised to indicate ~return was called.  Local so that the exception is tied to a
       particular call of [with_return]. *)
    exception Return of a
  end in
  let is_alive = ref true in
  let return a =
    if not !is_alive
    then failwith "use of [return] from a [with_return] that already returned";
    Exn.raise_without_backtrace (M.Return a);
  in
  try
    let a = f { return } in
    is_alive := false;
    a
  with exn ->
    is_alive := false;
    match exn with
    | M.Return a -> a
    | _ -> raise exn
;;

TEST_MODULE "with_return" = struct
  let test_loop loop_limit jump_out =
    with_return (fun { return } ->
      for i = 0 to loop_limit do begin
        if i = jump_out then return (`Jumped_out i);
      end done;
      `Normal)
  ;;

  TEST = test_loop 5 10 = `Normal
  TEST = test_loop 10 5 = `Jumped_out 5
  TEST = test_loop 5 5  = `Jumped_out 5

  let test_nested outer inner =
    with_return (fun { return = return_outer } ->
      if outer = `Outer_jump then return_outer `Outer_jump;
      let inner_res =
        with_return (fun { return = return_inner } ->
          if inner = `Inner_jump_out_completely then return_outer `Inner_jump;
          if inner = `Inner_jump then return_inner `Inner_jump;
          `Inner_normal)
      in
      if outer = `Jump_with_inner then return_outer (`Outer_later_jump inner_res);
      `Outer_normal inner_res)
  ;;

  TEST = test_nested `Outer_jump `Inner_jump                = `Outer_jump
  TEST = test_nested `Outer_jump `Inner_jump_out_completely = `Outer_jump
  TEST = test_nested `Outer_jump `Foo                       = `Outer_jump

  TEST = test_nested `Jump_with_inner `Inner_jump_out_completely = `Inner_jump
  TEST = test_nested `Jump_with_inner `Inner_jump = `Outer_later_jump `Inner_jump
  TEST = test_nested `Jump_with_inner `Foo        = `Outer_later_jump `Inner_normal

  TEST = test_nested `Foo `Inner_jump_out_completely = `Inner_jump
  TEST = test_nested `Foo `Inner_jump = `Outer_normal `Inner_jump
  TEST = test_nested `Foo `Foo = `Outer_normal `Inner_normal
end

