
#include <glgui/glxgui.h>
#include <sys/time.h>  // gettimeofday()
#include <glgui/box.h>


#if 1
#define WINDOW_WIDTH  512
#define WINDOW_HEIGHT 256
#else
#define WINDOW_WIDTH  1024
#define WINDOW_HEIGHT 512
#endif


/**** X EVENT HANDLER ****/

// FIXME: decouple drag state and button routing from X event.

/* Translate X events to abstract events */
static void glxgui_handle_XEvent(void *ctx, XEvent *e) {
    struct glxgui *x = ctx;
    enum control_event ce;
    int but = button_none;
    switch(e->type) {
    case ButtonPress:
        but = button_left + (e->xbutton.button - Button1);
        ce = ce_press;
        break;
    case ButtonRelease:
        but = button_left + (e->xbutton.button - Button1);
        ce = ce_release;
        break;
    case MotionNotify:
        ce = ce_motion;
        break;
    case KeyRelease:
    case KeyPress:
        ce = (e->type == KeyPress) ? ce_press : ce_release;
        switch(e->xkey.keycode) {
        case 58:
            but = button_move;
            break;
        default:
            return;
        }
        break;
    default: return;
    }

    box_control_handle_event(x->box_control, ce,
                             e->xbutton.x, x->box_control->h - e->xbutton.y,
                             but);
}


/**** ZL objects ****/

void glxgui_init(struct glxgui *x, struct box_control *bc) {
    /* Clean this up a bit.  Thea idea is to be able to run the non-RT
       part of initialization in a LP thread. */
    bzero(x, sizeof(*x));
    x->box_control = bc;
}

void glxgui_start(struct glxgui *x) {
    LEAF_ASSERT(x->box_control);
    LEAF_ASSERT(x->box_control->boxes);
    x->xd = zl_xdisplay_new(":0");
    x->xw = zl_xwindow_new();
    zl_xwindow_resize(x->xw, x->box_control->w, x->box_control->h);
    zl_xwindow_cursor(x->xw, 1);
    x->glx = zl_glx_new();

    // Order is important!
    if (!zl_glx_open_on_display(x->glx, x->xw, x->xd)) {
        ZL_LOG("Can't open GLX");
        exit(1);
    }
    zl_xwindow_config(x->xw, x->xd);
}

void glxgui_tick(struct glxgui *x) {
    struct timeval tv_current = {};
    gettimeofday(&tv_current, NULL);
    int sec  = tv_current.tv_sec  - x->tv_last.tv_sec;
    int usec = tv_current.tv_usec - x->tv_last.tv_usec;
    x->tv_last = tv_current;
    if (usec < 0) {
        sec--;
        usec += 1000000;
    }
    usec += sec * 1000000;
    //fprintf(stderr, "%7d us  \r", usec);
    //ZL_LOG("frame %d", frame++);
    //usleep(10000);
    zl_glx_2d_display(x->glx, x->xw, box_control_draw_view, x->box_control);
    zl_xdisplay_route_events(x->xd);
    zl_xwindow_for_events(x->xw, glxgui_handle_XEvent, x);
}
void glxgui_stop(struct glxgui *x) {
    zl_xdisplay_unregister_window(x->xd, x->xw);
    zl_xwindow_free(x->xw);  x->xw  = NULL;
    zl_xdisplay_free(x->xd); x->xd  = NULL;
    zl_glx_free(x->glx);     x->glx = NULL;
}

void *glxgui_thread(void *ctx) {
    struct glxgui *x = ctx;
    glxgui_start(x);
    while (!x->shutdown) glxgui_tick(x);
    glxgui_stop(x);
    free(x);
    return NULL;
}

struct glxgui *glxgui_open(struct box_control *bc) {
    struct glxgui *x = malloc(sizeof(*x));
    glxgui_init(x, bc);

    /* Start thread in detached state: we don't want to wait in the
       procesing thread. */
    thread_create_detached(&x->thread, glxgui_thread, x);
    return x;
}

void glxgui_close(struct glxgui *x) {
    x->shutdown = 1;
    // pthread_join(&x->thread); // DONT BLOCK!
}
