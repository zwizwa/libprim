
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

    box_control_handle_event(&x->box_control, ce,
                             e->xbutton.x, x->h - e->xbutton.y,
                             but);
}


/**** ZL objects ****/

void glxgui_init(struct glxgui *x, int w, int h) {
    bzero(x, sizeof(*x));
    x->w = w;
    x->h = h;
    box_control_init(&x->box_control, w, h);
    x->xd = zl_xdisplay_new(":0");
    x->xw = zl_xwindow_new();
    zl_xwindow_resize(x->xw, w, h);
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
    zl_glx_2d_display(x->glx, x->xw, box_control_draw_view, &x->box_control);
    zl_xdisplay_route_events(x->xd);
    zl_xwindow_for_events(x->xw, glxgui_handle_XEvent, x);
}
void glxgui_cleanup(struct glxgui *x) {
    zl_xdisplay_unregister_window(x->xd, x->xw);
    zl_xwindow_free(x->xw);
    zl_xdisplay_free(x->xd);
    zl_glx_free(x->glx);
    bzero(x,sizeof(*x));
}

void *glxgui_thread(void *ctx) {
    struct glxgui *x = ctx;
    while (1) glxgui_tick(x);
}

struct glxgui *glxgui_open(int w, int h) {
    struct glxgui *x = malloc(sizeof(*x));
    glxgui_init(x, w, h);
    const pthread_attr_t *attr = NULL;
    pthread_create(&x->thread, attr, glxgui_thread, x);
    return x;
}

void glxgui_close(struct glxgui *x) {
    // FIXME
}
