//Provides: unix_gethostname
//Requires: caml_new_string
function unix_gethostname() {
  return caml_new_string('localhost');
}

//Provides: caml_hash_string
//Requires: caml_hash
function caml_hash_string(str) { return caml_hash(10,100,0,str); }

//Provides: caml_hash_double
//Requires: caml_hash
function caml_hash_double(num) { return caml_hash(10,100,0,num); }

//Provides: core_array_unsafe_float_blit
//Requires: caml_array_blit
function core_array_unsafe_float_blit(a1, i1, a2, i2, len) { 
  return caml_array_blit(a1, i1, a2, i2, len);
}

//Provides: core_array_unsafe_int_blit
//Requires: caml_array_blit
function core_array_unsafe_int_blit(a1, i1, a2, i2, len) { 
  return caml_array_blit(a1, i1, a2, i2, len);
}

//Provides: fixed_close_channel
//Requires: caml_ml_close_channel
function fixed_close_channel(num) { return caml_ml_close_channel(num); }

