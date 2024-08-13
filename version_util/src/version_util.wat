(module
   (import "env" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "env" "caml_read_file_content"
      (func $caml_read_file_content (param (ref eq)) (result (ref eq))))

   (type $string (array (mut i8)))

   (data $build_info "/static/build_info.sexp")

   (data $default_build_info
      "((username \"\")(hostname \"\")(kernel \"\")(build_time \"1970-01-01 00:00:00Z\")(x_library_inlining false)(portable_int63 true)(dynlinkable_code false)(ocaml_version \"\")(executable_path \"\")(build_system \"\"))")

   (func (export "generated_build_info") (param (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $caml_read_file_content
               (array.new_data $string $build_info
                  (i32.const 0) (i32.const 23))))
         (catch $ocaml_exception
            (drop (pop (ref eq)))
            (array.new_data $string $default_build_info
               (i32.const 0) (i32.const 200)))))

   (data $hg_version "/static/hg_version.out")
   (data $default_hg_version "NO_VERSION_UTIL")

   (func (export "generated_hg_version") (param (ref eq)) (result (ref eq))
      (try (result (ref eq))
         (do
            (call $caml_read_file_content
               (array.new_data $string $hg_version
                  (i32.const 0) (i32.const 22))))
         (catch $ocaml_exception
            (drop (pop (ref eq)))
            (array.new_data $string $default_hg_version
               (i32.const 0) (i32.const 15)))))
)
