#include <caml/memory.h>

extern double caml_stat_minor_words;
extern double caml_stat_promoted_words;
extern double caml_stat_major_words;

extern char *caml_young_ptr;
extern char *caml_young_end;

extern uintnat caml_allocated_words;

CAMLprim value core_kernel_gc_minor_words(value unit __attribute__((unused)))
{
  return (Val_long((long)
    (caml_stat_minor_words +
       (double) Wsize_bsize (caml_young_end - caml_young_ptr))));
}

CAMLprim value core_kernel_gc_major_words(value unit __attribute__((unused)))
{
  return Val_long((long)(caml_stat_major_words + (double) caml_allocated_words));
}

CAMLprim value core_kernel_gc_promoted_words(value unit __attribute__((unused)))
{
  return Val_long((long) caml_stat_promoted_words);
}
