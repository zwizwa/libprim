#ifndef _GLXGUI_H_
#define _GLXGUI_H_

#include <zl/xwindow.h>
#include <zl/glx.h>
#include <zl/config.h>
#include <glgui/box.h>
#include <sys/time.h>  // struct timeval
#include <pthread.h>

/* Connect some ZL / Posix objects to box_control */
struct glxgui {
    zl_xdisplay *xd;
    zl_xwindow *xw;
    zl_glx *glx;
    struct timeval tv_last;
    struct box_control box_control;
    pthread_t thread;
    int w, h;
};


struct glxgui *glxgui_open(int w, int h);
void glxgui_close(struct glxgui *x);

#endif // _GLXGUI_H_
