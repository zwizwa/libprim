#include "xwindow.h"
#include "glx.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <zl/config.h>
#include <sys/time.h>  // gettimeofday()

#define WIDTH  512
#define HEIGHT 256
#define SLIDER_PIXELS 5
#define SLIDER_BORDER 10
#define CLIP_LO 0
#define CLIP_HI 127 // stick to MIDI

#define CLIP clip
static inline int clip(int v) {
    return v > CLIP_HI ? CLIP_HI :
          (v < CLIP_LO ? CLIP_LO : v);
}

/* Simplicity is probably more important than efficiency here. */

/* All GUI zones are squares.  Since the GUI layout doesn't change,
   the event routing could be implemented as a flat list.

   What is interesting here is the difference between an ongoing edit
   (drag) and a committed edit.  This popped out naturally.

   Box is subclassed.  Since we have a very simple hierarchy, this is
   all just C and a bunch of red tape macros.
*/

struct box;
typedef void (*set_delta_t) (struct box *b, int dx, int dy);
typedef void (*commit_t)    (struct box *b);
typedef void (*draw_t)      (struct box *b);

/* Abstract the method list in a macro.  This makes it easy to not
   forget to add implementations when the list changes. */
#define METHOD_LIST(m) \
    m(set_delta) \
    m(commit) \
    m(draw)

struct box_class {
#define BOX_CLASS_MEMBER(m) m##_t m;
METHOD_LIST(BOX_CLASS_MEMBER)
};



struct box {
    bool focus;
    int x,y,w,h;
    const char *name;
    struct box_class *class;
};


/* OpenGL tools */
void gl_rect(int x0, int y0, int x1, int y1) {
    glBegin(GL_QUADS);
        glVertex2i(x0, y0);
        glVertex2i(x1, y0);
        glVertex2i(x1, y1);
        glVertex2i(x0, y1);
    glEnd();
}

/* 7-SEGMENT
     A
   F   B
     G
   E   C
     D
*/

#include "segment.h"


#define S_T  2
#define S_W 10
#define S_H 10


void segment_draw(struct slider *s, char s) {

    if (s&S_A) { gl_rect
    }

}


/* SLIDER */
struct slider {
    struct box box;
    int dv;
    int v;
};
void slider_set_delta(struct slider *s, int dx, int dy) {
    s->dv = dy;
}
void slider_commit(struct slider *s) {
    s->v += s->dv;
    s->dv = 0;
}
void slider_draw(struct slider *s) {
    int v = CLIP(s->v + s->dv);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

        glTranslatef(s->box.x, s->box.y, 0);

        /* Background square */
        if (s->box.focus) {
            int v1 = CLIP(v+20);
            glColor3ub(v1,v,v);
        }
        else {
            glColor3ub(v,v,v);
        }
        gl_rect(0,0,s->box.w,s->box.h);

        /* Slider bar */
        glColor3f(1,1,1);
        int p = SLIDER_PIXELS/2;
        int b = SLIDER_BORDER;
        gl_rect(b,            v - p,
                s->box.w - b, v + p);

    glPopMatrix();
}
struct box_class slider_class = {
#define BOX_SLIDER_MEMBER(m) .m = (m##_t)(slider_##m),
METHOD_LIST(BOX_SLIDER_MEMBER)
};


/* TOP ZONE: static structure */

#define DW (WIDTH/8)
#define DH (HEIGHT/2)

#define GUI_SLIDERS(m)  \
    m(A0,0,0)           \
    m(B0,1,0)           \
    m(C0,2,0)           \
    m(D0,3,0)           \
    m(E0,4,0)           \
    m(F0,5,0)           \
    m(G0,6,0)           \
    m(H0,7,0)           \
    m(A1,0,1)           \
    m(B1,1,1)           \
    m(C1,2,1)           \
    m(D1,3,1)           \
    m(E1,4,1)           \
    m(F1,5,1)           \
    m(G1,6,1)           \
    m(H1,7,1)

#define DEF_SLIDER(n,gx,gy) struct slider box_##n = {.box = {.name = #n, .x = gx*DW, .y = gy*DH, .w=DW, .h=DH, .class = &slider_class }};
#define REF_SLIDER(n,...)   (struct box*)&box_##n,

GUI_SLIDERS(DEF_SLIDER)
static struct box *boxes[] = {
    GUI_SLIDERS(REF_SLIDER)
    NULL
};
#define BOX_FOR(p,a) for(p=&a[0]; *p; p++)  // NULL terminated list of pointers


// All ranges are low inclusive, high exclusive
static bool in_range(int point, int start, int range) {
    return (point >= start) && (point < (start + range));
}
static bool in_box(struct box *b, int x, int y) {
    return in_range(x, b->x, b->w) && in_range(y, b->y, b->h);
}
struct box *find_box(int x, int y) {
    struct box **b;
    BOX_FOR(b, boxes) { if (in_box(*b, x, y)) return *b; }
    return NULL;
}
int box_index(struct box *_b) {
    struct box **b;
    BOX_FOR(b, boxes) { if (*b == _b) return b-boxes; }
    return -1;
}
void draw_view(void *ctx, int w, int h) {
    struct box **b;
    BOX_FOR(b, boxes) { (*b)->class->draw(*b); }
}


/**** Abstract event handler */
struct control_state {
    struct box *b0;  // box of last click
    int x0;          // coords ..
    int y0;
    struct box *b1;  // box of last focus
};
enum control_event {
    ce_press,
    ce_motion,
    ce_release,
};
static struct control_state control_state = {};

void update_focus(struct control_state *s, struct box *b) {
    if (s->b1) {
        s->b1->focus = false;
    }
    if (b) {
        b->focus = true;
    }
    s->b1 = b;
}

void handle_event(struct control_state *s,
                  enum control_event e, int x, int y,
                  int but) {
    struct box *b = find_box(x, y);
    switch(e) {
    case ce_press:
        if (but != 0) return;
        /* Record state of last press as it determines drag routing. */
        s->b0 = b;
        s->x0 = x;
        s->y0 = y;
    case ce_motion:
        if (s->b0) {
            int dx = x - s->x0;
            int dy = y - s->y0;
            // ZL_LOG("drag %s (%d, %d)", s->b0->name, dx, dy);
            s->b0->class->set_delta(s->b0, dx, dy);
        }
        else {
            update_focus(s, b);
            // ZL_LOG("motion (%d,%d) %s", x, y, b ? b->name : "<none>");
        }
        break;
    case ce_release:
        if (but != 0) return;
        /* Commit delta */
        if (s->b0) {
            s->b0->class->commit(s->b0);
            s->b0 = NULL;
        }
        else {
            /* Shouldn't happen. */
            ZL_LOG("spurious release");
        }
        update_focus(s, b);
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
    handle_event(ctx, ce, e->xbutton.x, HEIGHT - e->xbutton.y, but);
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
        fprintf(stderr, "%7d us  \r", usec);

        //ZL_LOG("frame %d", frame++);
        //usleep(10000);
        zl_glx_2d_display(glx, xw, draw_view, NULL);
        zl_xdisplay_route_events(xd);
        zl_xwindow_for_events(xw, handle_XEvent, &control_state);
    }

    zl_xdisplay_unregister_window(xd, xw);
    zl_xwindow_free(xw);
    zl_xdisplay_free(xd);
    zl_glx_free(glx);
    return 0;
}
