(* We implement a weak pointer as an ephemeron, where the ephemeron's key is the weak
   pointer and the ephemeron's data is a new heap block used to register a finalizer for
   when the ephemeron is cleared.

   In OCaml 4.02, we used a [Weak.t] to implement weak pointers, where the finalizer was
   directly attached to the value pointed to by the weak pointer.  That approach did not
   work in 4.03, because that changed things so a finalizer on a value in a weak set is
   run an entire major collection cycle before the weak pointer in the weak set is
   cleared, which would force values to stay alive for an additional major cycle.

   By attaching the finalizer to a new block in the ephemeron's data, the finalizer is run
   when the ephemeron is cleared and it does not keep alive the value pointed to by the
   weak pointer. *)

open! Import
open! Std_internal

module Ephemeron = Core_ephemeron
module Gc = Core_gc

type 'a t =
  { mutable did_notify_cleared : bool
  ; ephemeron                  : ('a, unit ref) Ephemeron.t
  ; thread_safe_after_cleared  : (unit -> unit) option }

let create ?thread_safe_after_cleared () =
  { did_notify_cleared        = false
  ; ephemeron                 = Ephemeron.create ()
  ; thread_safe_after_cleared }
;;

let get t = Ephemeron.get_key t.ephemeron

let sexp_of_t sexp_of_a t = [%sexp (get t : a Heap_block.t option)]

let is_none t = Ephemeron.is_key_none t.ephemeron
let is_some t = Ephemeron.is_key_some t.ephemeron

let set t block =
  match t.thread_safe_after_cleared with
  | None -> Ephemeron.set_key t.ephemeron (Some block)
  | Some thread_safe_after_cleared ->
    let unit_block = Heap_block.create_exn (ref ()) in
    (* We set the ephemeron's data before setting the key, because setting the data will
       cause any prior finalizers to leave the ephemeron alone.  In particular, an old
       finalizer could run when allocating [Some block] before setting the key.  But
       that finalizer will see the new data and do nothing. *)
    Ephemeron.set_data t.ephemeron (Some unit_block);
    Ephemeron.set_key t.ephemeron (Some block);
    t.did_notify_cleared <- false;
    Gc.Expert.add_finalizer unit_block (fun unit_block ->
      (* The finalizer on [unit_block] may be called before the ephemeron is cleared.
         We detect if that happens, and if so, clear the ephemeron ourselves. *)
      begin match Ephemeron.get_data t.ephemeron with
      | None -> ()
      | Some block ->
        if phys_equal block unit_block
        then (
          Ephemeron.set_key  t.ephemeron None;
          Ephemeron.set_data t.ephemeron None)
      end;
      if not t.did_notify_cleared && Ephemeron.is_data_none t.ephemeron
      then (
        t.did_notify_cleared <- true;
        thread_safe_after_cleared ()));
;;
