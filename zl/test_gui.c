#include "xwindow.h"
#include "glx.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <zl/config.h>

#define WIDTH  512
#define HEIGHT 256


/* Simplicity is probably more important than efficiency here. */

/* All GUI zones are squares.  Since the GUI layout doesn't change,
   the event routing could be implemented as a flat list. */
struct box;
typedef void (*handler)(struct box *b, int w, int y);
struct box {
    int value;  // later, connect to model
    const char *name;
    int x,y,w,h;
};

/* TOP ZONE: 4 squares */
#define A_SIZE(x) (sizeof(x) / sizeof((x)[0]))
#define A_FOR(p,a) for(p=&a[0]; p<&a[A_SIZE(a)]; p++)
static struct box top_box[] = {
    {.name = "A", .x = 0,        .y = 0,        .w = WIDTH/2, .h = HEIGHT/2 },
    {.name = "B", .x = WIDTH/2,  .y = 0,        .w = WIDTH/2, .h = HEIGHT/2 },
    {.name = "C", .x = 0,        .y = HEIGHT/2, .w = WIDTH/2, .h = HEIGHT/2 },
    {.name = "D", .x = WIDTH/2,  .y = HEIGHT/2, .w = WIDTH/2, .h = HEIGHT/2 },
};

// All ranges are low inclusive, high exclusive
static bool in_range(int point, int start, int range) {
    return (point >= start) && (point < (start + range));
}
static bool in_box(struct box *b, int x, int y) {
    return in_range(x, b->x, b->w) && in_range(y, b->y, b->h);
}
struct box *find_box(int x, int y) {
    struct box *b;
    A_FOR(b, top_box) { if (in_box(b, x, y)) return b; }
    return NULL;
}
int box_index(struct box *b) {
    int i = b - top_box;
    if ((i < 0) || (i > A_SIZE(top_box))) return -1;
    return i;
}


/* Implement the simplest possible router, so code is easy adapt. */
void handle_event(void *context, XEvent *e) {
    /* Last press state */
    static struct box *b0;
    static int x0;
    static int y0;

    int x = e->xbutton.x;
    int y = e->xbutton.y;
    struct box *b = find_box(x, y);
    switch(e->type) {
    case ButtonPress:
        /* Record state of last press as it determines drag routing. */
        b0 = b; x0 = x; y0 = y;
    case MotionNotify:
        if (b0) {
            ZL_LOG("drag %s (%d, %d)", b0->name, x-x0, y-y0);
        }
        else {
            ZL_LOG("motion (%d,%d) %s", x, y, b ? b->name : "<none>");
        }
        break;
    case ButtonRelease:
        b0 = NULL;
        break;
    default:
        break;
    }
}

int main(void) {

    zl_xdisplay *xd = zl_xdisplay_new(":0");
    zl_xwindow *xw = zl_xwindow_new();
    zl_xwindow_resize(xw, WIDTH, HEIGHT);
    zl_xwindow_cursor(xw, 1);
    zl_glx *glx = zl_glx_new();

    // Order is important!
    if (!zl_glx_open_on_display(glx, xw, xd)) {
        ZL_LOG("Can't open GLX");
        exit(1);
    }
    zl_xwindow_config(xw, xd);

    unsigned char *video_data =
        zl_glx_image_data(glx, xw, WIDTH, HEIGHT);

    int frame = 0;
    while (1) {
        // ZL_LOG("frame %d", frame++);
        usleep(50000);
        unsigned char v = random();
        int i,nb_pixels = 3 * WIDTH * HEIGHT;
        for (i = 0; i<nb_pixels; i++) {
            video_data[i] = v++;
        }
        if (NULL == video_data) {
            ZL_LOG("video_data == NULL");
        } else {
            zl_glx_image_display(glx, xw);
            zl_xdisplay_route_events(xd);
            zl_xwindow_for_events(xw, handle_event, NULL);
        }
    }

    zl_xdisplay_unregister_window(xd, xw);
    zl_xwindow_free(xw);
    zl_xdisplay_free(xd);
    zl_glx_free(glx);
    return 0;
}
