#ifndef _GLXGUI_H_
#define _GLXGUI_H_

#include <zl/xwindow.h>
#include <zl/glx.h>
#include <zl/config.h>
#include <glgui/box.h>
#include <sys/time.h>  // struct timeval
#include <config.h>

typedef char atomic_t;  // FIXME

/* Connect some ZL / Posix objects to box_control */
struct glxgui {
    zl_xdisplay *xd;
    zl_xwindow *xw;
    zl_glx *glx;
    struct timeval tv_last;
    thread_t thread;
    atomic_t shutdown;

    /* This object is managed elsewhere.  We don't have a pointer to
       its destructor.  FIXME: Use LEAF RC. */
    struct box_control *box_control;
};


struct glxgui *glxgui_open(struct box_control *bc);
void glxgui_close(struct glxgui *x);

#endif // _GLXGUI_H_
