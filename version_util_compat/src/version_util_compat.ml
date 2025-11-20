open Core
module Time = Time_float

include
  (* Within Jane Street, this calls into a library that exposes version information
     embedded into binaries.

     The build rules that embed this information are only available within Jane Street's
     build system. *)
  struct
  let version = "NO_VERSION_UTIL"
  let version_list = [ version ]
  let reprint_build_info (_ : Time.t -> Sexp.t) = version
end

let x_library_inlining =
  (* The build rule that embeds build info like [x_library_inlining] is not available
     externally, so [Version_util.x_library_inlining] previously always returned [false]
     (the default value when no build info is embedded). This has the same behaviour, but
     allows us to not publish the [version_util] library publicly. *)
  false
;;
