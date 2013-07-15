(** This module is a grab bag.  Originally, it was the imports used in all the Zero
    library modules.  One nice thing it does is to remove polymorphic comparison
    functions.  This module doesn't make so much sense now that Zero is in Core.
    We're trying to figure out what to do with it. *)


open Std_internal

let _squelch_unused_module_warning_ = ()

let concat = String.concat
let is_error = Result.is_error
let is_ok    = Result.is_ok

let does_fail f = is_error (Result.try_with f)

include Int.Replace_polymorphic_compare

module Poly = Polymorphic_compare

