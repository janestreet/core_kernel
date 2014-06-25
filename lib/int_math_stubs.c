#include <stdlib.h>
#include <stdbool.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

static int64_t int_pow(int64_t base, int64_t exponent) {
  int64_t ret = 1;

  while(exponent != 0) {
    if((exponent & 1) != 0)
      ret *= base;
    base *= base;
    exponent >>= 1;
  }

  return ret;
}

CAMLprim value int_math_int_pow_stub(value base, value exponent) {
  return (Val_long(int_pow(Long_val(base), Long_val(exponent))));
}

CAMLprim value int_math_int64_pow_stub(value base, value exponent) {
  CAMLparam2(base, exponent);
  CAMLreturn(caml_copy_int64(int_pow(Int64_val(base), Int64_val(exponent))));
}
