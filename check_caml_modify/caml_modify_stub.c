#include <caml/mlvalues.h>

CAMLextern void __real_caml_modify(value *fp, value v);

static long count = 0;

CAMLprim void __wrap_caml_modify(value *fp, value v)
{
  count++;
  __real_caml_modify(fp, v);
}

CAMLprim value check_caml_modify_count()
{
  return Val_long(count);
}

CAMLprim value check_caml_modify_reset()
{
  count = 0;
  return Val_unit;
}
