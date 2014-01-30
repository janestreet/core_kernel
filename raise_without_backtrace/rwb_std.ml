(* This file is compiled with -inline 0 and without -g. This ensures that using the
   following function forces the fast version of raise that does not record the
   backtrace. *)


let raise_without_backtrace e = raise e
