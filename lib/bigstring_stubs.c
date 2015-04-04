#define _FILE_OFFSET_BITS 64

#define _GNU_SOURCE             /* recvmmsg */

/* For pread/pwrite */
#define _XOPEN_SOURCE 500

/* For OpenBSD `swap` functions */
#ifdef __OpenBSD__
#define _BSD_SOURCE
#endif

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <stdint.h>

#ifdef __APPLE__
#include <libkern/OSByteOrder.h>
#define bswap_16 OSSwapInt16
#define bswap_32 OSSwapInt32
#define bswap_64 OSSwapInt64
#elif __GLIBC__
#include <byteswap.h>
#include <malloc.h>
#elif __OpenBSD__
#include <sys/types.h>
#define bswap_16 swap16
#define bswap_32 swap32
#define bswap_64 swap64
#else
#include <sys/types.h>
#include <sys/endian.h>
#define __BYTE_ORDER    _BYTE_ORDER
#define __LITTLE_ENDIAN _LITTLE_ENDIAN
#define __BIG_ENDIAN    _BIG_ENDIAN
#define bswap_16 bswap16
#define bswap_32 bswap32
#define bswap_64 bswap64
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include "core_params.h"
#include "core_bigstring.h"

static inline char * get_bstr(value v_bstr, value v_pos)
{
  return (char *) Caml_ba_data_val(v_bstr) + Long_val(v_pos);
}

CAMLexport value
bigstring_alloc (value v_gc_max_unused, value v_size)
{
  intnat size = Long_val (v_size);
  void * data = NULL;
  int flags = CORE_BIGSTRING_FLAGS | CAML_BA_MANAGED;
  intnat gc_max_unused = Long_val(v_gc_max_unused);
  intnat dims[1];
  dims[0] = size;

  if (gc_max_unused >= 0) {
    data = (void *) malloc(sizeof(char) * size);
    if (NULL == data) caml_raise_out_of_memory ();
    /* caml_adjust_gc_speed is also called by caml_ba_alloc below, but it will have
    * numerator 0 when data != NULL. Effectively, that call will have no effect if this
    * call is made. */
    caml_adjust_gc_speed(size, gc_max_unused);
  }

  return caml_ba_alloc (flags, 1, data, dims);
}

/* Checking memory-mapping */

CAMLprim value bigstring_is_mmapped_stub(value v_bstr)
{
  return
    Val_bool((Caml_ba_array_val(v_bstr)->flags & CAML_BA_MAPPED_FILE) != 0);
}

/* Blitting */

CAMLprim value bigstring_blit_string_bigstring_stub(
  value v_str, value v_src_pos, value v_bstr, value v_dst_pos, value v_len)
{
  char *str = String_val(v_str) + Long_val(v_src_pos);
  char *bstr = get_bstr(v_bstr, v_dst_pos);
  memcpy(bstr, str, Long_val(v_len));
  return Val_unit;
}

CAMLprim value bigstring_blit_bigstring_string_stub(
  value v_bstr, value v_src_pos, value v_str, value v_dst_pos, value v_len)
{
  char *bstr = get_bstr(v_bstr, v_src_pos);
  char *str = String_val(v_str) + Long_val(v_dst_pos);
  memcpy(str, bstr, Long_val(v_len));
  return Val_unit;
}

CAMLprim value bigstring_blit_stub(
  value v_src, value v_src_pos, value v_dst, value v_dst_pos, value v_len)
{
  struct caml_ba_array *ba_src = Caml_ba_array_val(v_src);
  struct caml_ba_array *ba_dst = Caml_ba_array_val(v_dst);
  char *src = (char *) ba_src->data + Long_val(v_src_pos);
  char *dst = (char *) ba_dst->data + Long_val(v_dst_pos);
  size_t len = Long_val(v_len);
  if (len > THREAD_IO_CUTOFF)
  {
    Begin_roots2(v_src, v_dst);
    caml_enter_blocking_section();
      memmove(dst, src, Long_val(v_len));
    caml_leave_blocking_section();
    End_roots();
  }
  else memmove(dst, src, Long_val(v_len));
  return Val_unit;
}

/* Search */

CAMLprim value bigstring_find(value v_str, value v_needle,
                              value v_pos, value v_len)
{
  char *start, *r;
  intnat ret;

  start = get_bstr(v_str, v_pos);
  r = (char*) memchr(start, Int_val(v_needle), Long_val(v_len));

  if (!r) return Val_long(-1);

  ret = Long_val(v_pos) + r - start;
  return Val_long(ret);
}

/* Destruction */

static void check_bigstring_proxy(struct caml_ba_array *b)
{
  if (b->proxy != NULL) caml_failwith("bigstring_destroy: bigstring has proxy");
}

extern void caml_ba_unmap_file(void *addr, uintnat len);

void core_bigstring_destroy(struct caml_ba_array *b, int flags)
{
  int i;
  switch (b->flags & CAML_BA_MANAGED_MASK) {
    case CAML_BA_EXTERNAL :
      if ((flags & CORE_BIGSTRING_DESTROY_ALLOW_EXTERNAL)
           != CORE_BIGSTRING_DESTROY_ALLOW_EXTERNAL)
        caml_failwith("bigstring_destroy: bigstring is external or already deallocated");
      break;
    case CAML_BA_MANAGED :
      check_bigstring_proxy(b);
      free(b->data);
      break;
    case CAML_BA_MAPPED_FILE :
      check_bigstring_proxy(b);
      if ((flags & CORE_BIGSTRING_DESTROY_DO_NOT_UNMAP)
           != CORE_BIGSTRING_DESTROY_DO_NOT_UNMAP)
        caml_ba_unmap_file(b->data, caml_ba_byte_size(b));
      break;
  }
  b->data = NULL;
  b->flags = CAML_BA_EXTERNAL;
  for (i = 0; i < b->num_dims; ++i) b->dim[i] = 0;
}

CAMLprim value bigstring_destroy_stub(value v_bstr)
{
  core_bigstring_destroy(Caml_ba_array_val(v_bstr), 0);
  return Val_unit;
}
