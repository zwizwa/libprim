#include <zl/xwindow.h>
#include <zl/glx.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <zl/config.h>
#include <sys/time.h>  // gettimeofday()
#include <math.h>

#include <glgui/segment.h>
#include <glgui/render_spec.h>
#include <glgui/box.h>


#define WINDOW_WIDTH  512
#define WINDOW_HEIGHT 256



/**** X EVENT HANDLER ****/

// FIXME: decouple drag state and button routing from X event.

/* Translate X events to abstract events */
void handle_XEvent(void *ctx, XEvent *e) {
    enum control_event ce;
    int but;
    switch(e->type) {
    case ButtonPress:
        but = e->xbutton.button - Button1;
        ce = ce_press;
        break;
    case ButtonRelease:
        but = e->xbutton.button - Button1;
        ce = ce_release;
        break;
    case MotionNotify:
        but = -1;
        ce = ce_motion;
        break;
    default: return;
    }

    // FIXME: get height from window, or keep fixed?
    box_control_handle_event(ctx, ce, e->xbutton.x,
                             WINDOW_HEIGHT - e->xbutton.y, but);
}


/**** ZL objects ****/

void zl_glx_2d_display(zl_glx *x, zl_xwindow_p xwin,
                       void (*draw)(void*,int,int), void *ctx);
int main(void) {

    struct box_control box_control;
    box_control_init(&box_control, WINDOW_WIDTH, WINDOW_HEIGHT);

    zl_xdisplay *xd = zl_xdisplay_new(":0");
    zl_xwindow *xw = zl_xwindow_new();
    zl_xwindow_resize(xw, WINDOW_WIDTH, WINDOW_HEIGHT);
    zl_xwindow_cursor(xw, 1);
    zl_glx *glx = zl_glx_new();

    // Order is important!
    if (!zl_glx_open_on_display(glx, xw, xd)) {
        ZL_LOG("Can't open GLX");
        exit(1);
    }
    zl_xwindow_config(xw, xd);
    // zl_glx_vsync(glx, false);

    // int frame = 0;
    struct timeval tv_last = {};
    while (1) {
        struct timeval tv_current = {};
        gettimeofday(&tv_current, NULL);
        int sec  = tv_current.tv_sec  - tv_last.tv_sec;
        int usec = tv_current.tv_usec - tv_last.tv_usec;
        tv_last = tv_current;
        if (usec < 0) {
            sec--;
            usec += 1000000;
        }
        usec += sec * 1000000;
        // fprintf(stderr, "%7d us  \r", usec);

        //ZL_LOG("frame %d", frame++);
        //usleep(10000);
        zl_glx_2d_display(glx, xw, box_control_draw_view, &box_control);
        zl_xdisplay_route_events(xd);
        zl_xwindow_for_events(xw, handle_XEvent, &box_control);
    }

    zl_xdisplay_unregister_window(xd, xw);
    zl_xwindow_free(xw);
    zl_xdisplay_free(xd);
    zl_glx_free(glx);
    return 0;
}
