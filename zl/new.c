/* malloc / free wrappers for abstract access. */

#include "v4l.h"
#include "xwindow.h"
#include "xv.h"
#include "3Dcontext.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

struct zl_v4l *zl_v4l_new(bool start_thread) {
  struct zl_v4l *x = malloc(sizeof(*x));
  zl_v4l_init(x, start_thread);
  return x;
}
