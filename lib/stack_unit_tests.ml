open Std_internal

let is_error = Result.is_error
let is_ok    = Result.is_ok

module Debug (Stack : Stack_intf.S) : Stack_intf.S with type 'a t = 'a Stack.t = struct

  open Stack

  type nonrec 'a t = 'a t with bin_io

  let invariant = invariant

  let check_and_return t = invariant ignore t; t

  let debug t f =
    let result = Result.try_with f in
    invariant ignore t;
    Result.ok_exn result;
  ;;

  (* The return-type annotations are to prevent an error where we don't supply all the
     arguments to the function, and thus wouldn't be checking the invariant after fully
     applying the function. *)
  let clear t : unit                 = debug t (fun () -> clear t)
  let copy t : _ t                   = check_and_return (debug t (fun () -> copy t))
  let count t ~f : int               = debug t (fun () -> count t ~f)
  let create () : _ t                = check_and_return (create ())
  let exists t ~f : bool             = debug t (fun () -> exists t ~f)
  let find t ~f : _ option           = debug t (fun () -> find t ~f)
  let find_map t ~f : _ option       = debug t (fun () -> find_map t ~f)
  let fold (type a) t ~init ~f : a   = debug t (fun () -> fold t ~init ~f)
  let for_all t ~f : bool            = debug t (fun () -> for_all t ~f)
  let is_empty t : bool              = debug t (fun () -> is_empty t)
  let iter t ~f : unit               = debug t (fun () -> iter t ~f)
  let length t : int                 = debug t (fun () -> length t)
  let mem ?equal t a : bool          = debug t (fun () -> mem ?equal t a)
  let of_list l : _ t                = check_and_return (of_list l)
  let pop t : _ option               = debug t (fun () -> pop t)
  let pop_exn (type a) t : a         = debug t (fun () -> pop_exn t)
  let push t a : unit                = debug t (fun () -> push t a)
  let sexp_of_t sexp_of_a t : Sexp.t = debug t (fun () -> <:sexp_of< a t >> t)
  let t_of_sexp a_of_sexp sexp : _ t = check_and_return (<:of_sexp< a t >> sexp)
  let to_array t : _ array           = debug t (fun () -> to_array t)
  let to_list t : _ list             = debug t (fun () -> to_list t)
  let top t : _ option               = debug t (fun () -> top t)
  let top_exn (type a) t : a         = debug t (fun () -> top_exn t)
  let until_empty t f : unit         = debug t (fun () -> until_empty t f)

end

module Test (Stack : Stack_intf.S)
  (* This signature is here to remind us to add a unit test whenever we add something to
     the stack interface. *)
  : Stack_intf.S with type 'a t = 'a Stack.t = struct

  open Stack

  type nonrec 'a t = 'a t with bin_io

  include Container_unit_tests.Test_S1 (Stack)

  let invariant = invariant

  let create = create
  let is_empty = is_empty
  let top_exn = top_exn
  let pop_exn = pop_exn
  let pop = pop
  let top = top

  TEST_UNIT =
    let empty = create () in
    invariant ignore empty;
    invariant (fun b -> assert b) (of_list [true]);
    assert (is_empty empty);
    let t = create () in
    push t 0;
    assert (not (is_empty t));
    assert (try ignore (top_exn empty); false with _ -> true);
    let t = create () in
    push t 0;
    assert (top_exn t = 0);
    assert (try ignore (pop_exn empty); false with _ -> true);
    let t = create () in
    push t 0;
    assert (pop_exn t = 0);
    assert (is_none (pop empty));
    assert (is_some (pop (of_list [0])));
    assert (is_none (top empty));
    assert (is_some (top (of_list [0])));
  ;;

  let push = push
  let copy = copy
  let until_empty = until_empty

  TEST_UNIT =
    let t =
      let t = create () in
      push t 0;
      push t 1;
      push t 2;
      t
    in
    assert (not (is_empty t));
    assert (length t = 3);
    assert (top t = Some 2);
    assert (top_exn t = 2);
    let t' = copy t in
    assert (pop_exn t' = 2);
    assert (pop_exn t' = 1);
    assert (pop_exn t' = 0);
    assert (length t' = 0);
    assert (is_empty t');
    let t' = copy t in
    assert (pop t' = Some 2);
    assert (pop t' = Some 1);
    assert (pop t' = Some 0);
    assert (length t' = 0);
    assert (is_empty t');
    (* test that t was not modified by pops applied to copies *)
    assert (length t = 3);
    assert (top_exn t = 2);
    assert (to_list t = [2; 1; 0]);
    assert (to_array t = [|2; 1; 0|]);
    assert (length t = 3);
    assert (top_exn t = 2);
    let t' = copy t in
    let n = ref 0 in
    until_empty t' (fun x -> n := !n + x);
    assert (!n = 3);
    assert (is_empty t');
    assert (length t' = 0);
  ;;

  TEST_UNIT =
    let t = create () in
    assert (is_empty t);
    assert (length t = 0);
    assert (to_list t = []);
    assert (is_none (pop t));
    push t 13;
    assert (not (is_empty t));
    assert (length t = 1);
    assert (pop_exn t = 13);
    assert (is_empty t);
    assert (length t = 0);
    push t 13;
    push t 14;
    assert (not (is_empty t));
    assert (length t = 2);
    assert (to_list t = [ 14; 13 ]);
    assert (is_some (pop t));
    assert (is_some (pop t));
  ;;

  let of_list = of_list

  TEST_UNIT =
    for n = 0 to 5 do
      let l = List.init n ~f:Fn.id in
      assert (l = to_list (of_list l));
    done;
  ;;

  let clear = clear

  TEST_UNIT =
    for n = 0 to 5 do
      let t = of_list (List.init n ~f:Fn.id) in
      clear t;
      assert (is_empty t);
      push t 13;
      assert (length t = 1);
    done;
  ;;

end

include (Test (Debug (Linked_stack)) : sig end)

TEST_MODULE = (struct

  open Core_stack

  include Test (Debug (Core_stack))

  let capacity     = capacity
  let set_capacity = set_capacity

  TEST_UNIT =
    let t = create () in
    assert (capacity t = 0);
    set_capacity t (-1);
    assert (capacity t = 0);
    set_capacity t 10;
    assert (capacity t = 10);
    set_capacity t 0;
    assert (capacity t = 0);
    push t ();
    set_capacity t 0;
    assert (length t = 1);
    assert (capacity t >= 1);
  ;;
end
(* This signature constraint is here to remind us to add a unit test whenever the
   interface to [Core_stack] changes. *)
: module type of Core_stack)
