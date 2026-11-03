open! Core

(* [Sys.opaque_identity] on the input keeps [of_string] from being constant-folded and
   keeps the result live. *)
let uuid = "c14ac950-b432-46ed-7d19-622539d60705"

let%bench_fun "Uuid.of_string" =
  let s = Sys.opaque_identity uuid in
  fun () -> ignore (Sys.opaque_identity (Uuid.of_string s) : Uuid.t)
;;
