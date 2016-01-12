let package_name = "core_kernel"

let sections =
  [ ("lib",
    [ ("built_lib_core_kernel", None)
    ],
    [ ("META", None)
    ; ("include/core_params.h", None)
    ; ("include/jane_common.h", None)
    ; ("include/ocaml_utils.h", None)
    ; ("include/unix_utils.h", None)
    ; ("src/time_ns_stubs.h", None)
    ])
  ]
