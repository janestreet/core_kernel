(* Check that [Version_util_compat] implements [Command]'s [Version_util] interface. *)
module _ : Command__Command_intf.Version_util = Version_util_compat
