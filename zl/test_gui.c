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
    bool focus;
    int v;  // later, connect to model
    int dv; // current delta (interesting!)
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


/**** Abstract event handler */
struct control_state {
    struct box *b0;  // box of last click
    int x0;          // coords ..
    int y0;
};
enum control_event {
    ce_press,
    ce_motion,
    ce_release,
};
static struct control_state control_state = {};

void handle_event(struct control_state *s,
                  enum control_event e, int x, int y) {
    struct box *b = find_box(x, y);
    switch(e) {
    case ce_press:
        /* Record state of last press as it determines drag routing. */
        s->b0 = b;
        s->x0 = x;
        s->y0 = y;
    case ce_motion:
        if (s->b0) {
            int dx = x - s->x0;
            int dy = y - s->y0;
            ZL_LOG("drag %s (%d, %d)", s->b0->name, dx, dy);
            s->b0->dv = dy;
        }
        else {
            ZL_LOG("motion (%d,%d) %s", x, y, b ? b->name : "<none>");
        }
        break;
    case ce_release:
        /* Commit delta */
        s->b0->v += s->b0->dv;
        s->b0->dv = 0;
        s->b0 = NULL;
        break;
    default:
        break;
    }
}



/**** X EVENT HANDLER ****/

// FIXME: decouple drag state and button routing from X event.

/* Translate X events to abstract events */
void handle_XEvent(void *ctx, XEvent *e) {
    enum control_event ce;
    switch(e->type) {
    case ButtonPress:   ce = ce_press;   break;
    case MotionNotify:  ce = ce_motion;  break;
    case ButtonRelease: ce = ce_release; break;
    default: return;
    }
    handle_event(ctx, ce, e->xbutton.x, HEIGHT - e->xbutton.y);
}

/**** VIEW ****/
void draw_box(struct box *b) {
    int v = b->v + b->dv;
    glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glTranslatef(b->x, b->y, 0);
        ZL_LOG("%s %d", b->name, v);
        glColor3ub(v,v,v);
        glBegin(GL_QUADS);
            glVertex2i(b->w,0);
            glVertex2i(b->w, b->h);
            glVertex2i(0, b->h);
            glVertex2i(0,0);
        glEnd();
    glPopMatrix();
}
void draw_view(void *ctx, int w, int h) {
    struct box *b;
    A_FOR(b, top_box) { draw_box(b); }
}


/**** ZL objects ****/

void zl_glx_2d_display(zl_glx *x, zl_xwindow_p xwin,
                       void (*draw)(void*,int,int), void *ctx);
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
            zl_glx_2d_display(glx, xw, draw_view, NULL);
            zl_xdisplay_route_events(xd);
            zl_xwindow_for_events(xw, handle_XEvent, &control_state);
        }
    }

    zl_xdisplay_unregister_window(xd, xw);
    zl_xwindow_free(xw);
    zl_xdisplay_free(xd);
    zl_glx_free(glx);
    return 0;
}
