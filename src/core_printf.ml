open! Import

include Base.Printf

let eprintf  = Stdio.Out_channel.eprintf
let fprintf  = Stdio.Out_channel.fprintf
let kfprintf = Stdio.Out_channel.kfprintf
let printf   = Stdio.Out_channel.printf

(** print to stderr; exit 1 *)
let exitf fmt = ksprintf (fun s () -> Printf.eprintf "%s\n%!" s; exit 1) fmt
