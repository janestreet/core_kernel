#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

/* Fixing 5193 */

/* Fix the broken close_(in/out) function which does not release the
   caml lock. */

#define IO_BUFFER_SIZE 65536

typedef long file_offset;

struct channel {
  int fd;                       /* Unix file descriptor */
  file_offset offset;           /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel * next, * prev;/* Double chaining of channels (flush_all) */
  int revealed;                 /* For Cash only */
  int old_revealed;             /* For Cash only */
  int refcount;                 /* For flush_all and for Cash */
  int flags;                    /* Bitfield */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

CAMLprim value fixed_close_channel(value vchannel)
{
  int result;
  int tmp_fd = -1;
  int tries = 0;
  struct channel *channel = Channel(vchannel);

  if (channel->fd != -1) {
    tmp_fd = channel->fd;
    channel->fd = -1;

    caml_enter_blocking_section();
    do {
      tries++;
      result = close(tmp_fd);
    } while(result == -1 && (errno == EINTR || errno == EAGAIN) && tries < 1000);
    caml_leave_blocking_section();

    if(result == -1) {
      channel->fd = tmp_fd;
      uerror("error closing channel", Nothing);
    } else
      channel->curr = channel->max = channel->end;
  }

  return Val_unit;
}
