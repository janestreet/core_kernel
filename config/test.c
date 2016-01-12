/* This file is just preprocessed.  Lines of the form "OUT:XXX" are
   kept and replaced by XXX in the output to produce
   lib/config.mlh. */

#include <caml/config.h>

/* Defined in <caml/config.h> */
#if defined(ARCH_SIXTYFOUR)
"OUT:#let JSC_ARCH_SIXTYFOUR = true"
#else
"OUT:#let JSC_ARCH_SIXTYFOUR = false"
#endif

/* Defined in <caml/config.h> */
#if defined(ARCH_BIG_ENDIAN)
"OUT:#let JSC_ARCH_BIG_ENDIAN = true"
#else
"OUT:#let JSC_ARCH_BIG_ENDIAN = false"
#endif

#if defined(POSIX_TIMERS)
"OUT:#let JSC_POSIX_TIMERS = true"
#else
"OUT:#let JSC_POSIX_TIMERS = false"
#endif
