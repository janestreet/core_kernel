(module
   (type $string (array (mut i8)))

   (global $generated_build_info_global (mut (ref $string))
      (array.new_fixed $string 0))

   (global $generated_hg_version_global (mut (ref $string))
      (array.new_fixed $string 0))

   (func (export "generated_build_info") (param (ref eq)) (result (ref eq))
      (global.get $generated_build_info_global))

   (func (export "generated_hg_version") (param (ref eq)) (result (ref eq))
      (global.get $generated_hg_version_global))

   (func (export "set_generated_build_info") (param $build_info (ref $string))
      (global.set $generated_build_info_global (local.get $build_info)))

   (func (export "set_generated_hg_version") (param $hg_version (ref $string))
      (global.set $generated_hg_version_global (local.get $hg_version)))
)
