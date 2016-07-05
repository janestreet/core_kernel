open! Std_internal

module Gc   = Core_gc
module Weak = Core_weak

type 'a t =
  { mutable did_notify_cleared : bool
  ; thread_safe_after_cleared  : (unit -> unit) option
  ; weak                       : 'a Weak.t }

let create ?thread_safe_after_cleared () =
  { did_notify_cleared        = false
  ; thread_safe_after_cleared
  ; weak                      = Weak.create ~len:1 }
;;

let get t = Weak.get t.weak 0

let sexp_of_t sexp_of_a t = [%sexp (get t : a Heap_block.t option)]

let is_none t = Weak.is_none t.weak 0
let is_some t = Weak.is_some t.weak 0

let set t block =
  Weak.set t.weak 0 (Some block);
  match t.thread_safe_after_cleared with
  | None -> ()
  | Some thread_safe_after_cleared ->
    t.did_notify_cleared <- false;
    Gc.Expert.add_finalizer block (fun _ ->
      if not t.did_notify_cleared && is_none t
      then (
        t.did_notify_cleared <- true;
        thread_safe_after_cleared ()));
;;
