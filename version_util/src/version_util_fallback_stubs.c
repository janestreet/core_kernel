/* version_util.ml refers to this. This allows any library that depends on this one to be
   installed as a standalone library, e.g. for use in a toplevel. Our build rules, when
   building an executable, generate replacement functions with real information in
   *.build_info.c and *.hg_version.c. The latter ones have precedence as the ones in this
   file are defined as weak. And because all of this is C functinos, we can replace this
   information without doing a ton of ocaml compilation (essentially just relinking). */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#if defined(_MSC_VER) && _MSC_VER >= 1500
# define __unused(x) __pragma( warning (push) ) \
    __pragma( warning (disable:4189 ) ) \
    x \
    __pragma( warning (pop))
#else
# define __unused(x) x __attribute__((unused))
#endif

CAMLprim CAMLweakdef value generated_hg_version (__unused(value unit))
{
  return(caml_copy_string(""));
}

CAMLprim CAMLweakdef value generated_build_info (__unused(value unit))
{
  return(caml_copy_string(""));
}
