Core is an industrial-strength alternative to the OCaml standard
library.  It was developed by Jane Street, which is the largest
industrial user of OCaml.

Core_kernel is the system-independent part of Core.  It is aimed for
cases when the full Core is not available, such as in Javascript.  It
provides an overlay on the usual namespace, so the best way to use
Core is to start your file with:

   open Core_kernel.Std

In the case of bugs, feature requests and similar, you can contact us
at <opensource@janestreet.com>.  You can find all of Jane Street's
open-source libraries on GitHub <https://github.com/janestreet>.
