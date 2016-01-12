Portable standard library for OCaml
===================================

Core is an industrial-strength alternative to the OCaml standard
library.  It was developed by Jane Street, which is the largest
industrial user of OCaml.

Core\_kernel is the system-independent part of Core.  It is aimed for
cases when the full Core is not available, such as in Javascript.  It
provides an overlay on the usual namespace, so the best way to use
Core is to start your file with:

```ocaml
open Core_kernel.Std
```

Please report bugs and feature requests on
[GitHub](https://github.com/janestreet/core_kernel). For everything
else you can contact us at <ocaml-core@googlegroups.com>.

You can find all of Jane Street's open-source libraries on
[GitHub](https://github.com/janestreet).
