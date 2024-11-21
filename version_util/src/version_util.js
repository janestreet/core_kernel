//Provides: generated_build_info
//Requires: caml_read_file_content, caml_string_of_jsbytes
function generated_build_info() {
  try {
    return caml_read_file_content("/static/build_info.sexp");
  } catch (e) {
    return caml_string_of_jsbytes("");
  }
}


//Provides: generated_hg_version
//Requires: caml_read_file_content, caml_string_of_jsbytes
function generated_hg_version() {
  try {
    return caml_read_file_content("/static/hg_version.out");
  } catch (e) {
    return caml_string_of_jsbytes("");
  }
}
