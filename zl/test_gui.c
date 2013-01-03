#include "xwindow.h"
#include "glx.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <zl/config.h>
#include <sys/time.h>  // gettimeofday()

#include "segment.h"

/* 7-segment size */
#define S_T  1 // thickness
#define S_W  3 // horizontal segment width
#define S_H  2 // vertical segment height

#define WINDOW_WIDTH  512
#define WINDOW_HEIGHT 256

#define SLIDER_NX 8
#define SLIDER_NY 2
#define SLIDER_PIXELS 5
#define SLIDER_BORDER 10
#define SLIDER_MARGIN 0

// stick to MIDI
#define CLIP_LO 0
#define CLIP_HI 127

#define CLIP clip
static inline int clip(int v) {
    return v > CLIP_HI ? CLIP_HI :
          (v < CLIP_LO ? CLIP_LO : v);
}

#define WHEEL_UP   3
#define WHEEL_DOWN 4

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

    /* Static */
    const struct box_class *class;
    const char * name;
    int x,y,w,h;

    /* Dynamic */
    bool focus;
};


/* OpenGL tools */
static void gl_rect(int x0, int y0, int x1, int y1) {
    // ZL_LOG("gl_rect(%d,%d,%d,%d)", x0, y0, x1, y1);
    glBegin(GL_QUADS);
        glVertex2i(x0, y0);
        glVertex2i(x1, y0);
        glVertex2i(x1, y1);
        glVertex2i(x0, y1);
    glEnd();
}
static void gl_rect_w(int x, int y, int w, int h) {
    gl_rect(x,y,x+w,y+h);
}

/* 7-SEGMENT
     A
   F   B
     G
   E   C
     D
*/


static bool segment_color(bool on) {
    if (on) glColor3f(0.5,0,0);
    else    glColor3f(0.1,0,0);
    return on;
}
static void segment_draw(unsigned char s, int x0) {
    int w,h,x,y;

    /* Horizontal bars */
    x=S_T+x0;
    y=0;
    w=S_W;
    h=S_T;
    if (segment_color(s&S_D)) gl_rect_w(x,y,w,h); y += S_T+S_H;
    if (segment_color(s&S_G)) gl_rect_w(x,y,w,h); y += S_T+S_H;
    if (segment_color(s&S_A)) gl_rect_w(x,y,w,h);

    /* Left bars */
    x=x0;
    y=S_T;
    w=S_T;
    h=S_H;
    if (segment_color(s&S_E)) gl_rect_w(x,y,w,h); y += S_T+S_H;
    if (segment_color(s&S_F)) gl_rect_w(x,y,w,h);

    /* Right bars */
    x=S_T+S_W+x0;
    y=S_T;
    if (segment_color(s&S_C)) gl_rect_w(x,y,w,h); y += S_T+S_H;
    if (segment_color(s&S_B)) gl_rect_w(x,y,w,h);
}
const unsigned char digit_to_segment[10] = S_DIGITS_INIT;
static void segment_draw_digit(int digit, int x0) {
    if ((digit < 0) || (digit > 9)) return;
    segment_draw(digit_to_segment[digit], x0);
}

static void segment_draw_number(int number, int nb_digits) {
    int dx = S_T*3 + S_W;
    int x0 = dx * nb_digits;
    while(nb_digits--) {
        int rem = number % 10;
        number /= 10;
        x0 -= dx;
        segment_draw_digit(rem, x0);
    }
}


/* VARIABLE: a simple variable model capable of representing an
   in-progress edit as v+dv  */
struct variable {
    int v;
    int dv;
};
int variable_get(struct variable *var) {
    return var->v + var->dv;
}
void variable_set_delta(struct variable *var, int dv) {
    /* Keep v+dv in proper range. */
    int v = clip(var->v + dv);
    var->dv = v - var->v;
}
void variable_commit(struct variable *var) {
    var->v += var->dv;
    var->dv = 0;
}


/* SLIDER == a box connected to a variable model. */
struct slider {
    struct box box;
    struct variable *var;
};
void slider_set_delta(struct slider *s, int dx, int dy) {
    if (s->var) variable_set_delta(s->var, dy);
}
void slider_commit(struct slider *s) {
    if (s->var) variable_commit(s->var);
}
void slider_draw(struct slider *s) {
    int v = variable_get(s->var);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

        glTranslatef(s->box.x, s->box.y, 0);

        /* Background square */
        if (s->box.focus) {
            glColor3f(0.1,0.1,0.1);
            // int v1 = CLIP(v+20);
            // glColor3ub(v1,v,v);
        }
        else {
            glColor3f(0,0,0);
            // glColor3ub(v,v,v);
        }
        gl_rect(0,0,s->box.w,s->box.h);

        /* Level marks */
        int i;
        glColor3f(.3,.3,.3);
        for (i=0; i<11; i++) {
            int b = 15;
            int y = i*13;
            b -= ((i % 5) == 0) ? 5 : 0;
            gl_rect(b,            y,
                    s->box.w - b, y+2);
        }

        /* Slider bar */
        if (1) {
            glColor3f(.7,.7,.7);
            int p = SLIDER_PIXELS/2;
            int b = SLIDER_BORDER;
            gl_rect(b,            v - p,
                    s->box.w - b, v + p);
        }

        /* Numbers */
        if (1) {
            segment_draw_number(v, 3);
        }


    glPopMatrix();
}
struct box_class slider_class = {
#define BOX_SLIDER_MEMBER(m) .m = (m##_t)(slider_##m),
METHOD_LIST(BOX_SLIDER_MEMBER)
};


/* TOP ZONE: static structure */

struct box **boxes;
#define BOX_FOR(p,a) for(p=&a[0]; *p; p++)  // NULL terminated list of pointers

static void init_boxes(void) {
    int nx = SLIDER_NX;
    int ny = SLIDER_NY;
    int DW = WINDOW_WIDTH / nx;
    int DH = WINDOW_HEIGHT / ny;
    int x,y;
    int i = 0;

    struct variable *var[nx];
    for (x = 0; x < nx; x++) {
        var[x] = calloc(1, sizeof(var[x]));
        var[x]->v = 64;
    }
    boxes = calloc(1 + (nx * ny), sizeof(void *));
    for (y = 0; y < ny; y++) {
        for (x = 0; x < nx; x++) {
            struct slider *s = calloc(1, sizeof(*s));
            s->box.name = "?";
            s->box.x = x * DW + SLIDER_MARGIN;
            s->box.y = y * DH + SLIDER_MARGIN;
            s->box.w = DW - 2*SLIDER_MARGIN;
            s->box.h = DH - 2*SLIDER_MARGIN;
            s->box.class = &slider_class;
            s->var = var[x];
            boxes[i++] = &(s->box);
        }
    }
}

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
    glColor3f(0,0,0);
    gl_rect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);
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

static void box_inc(struct box *b, int inc) {
    if (b) {
        b->class->set_delta(b, 0, inc);
        b->class->commit(b);
    }
}

void handle_event(struct control_state *s,
                  enum control_event e, int x, int y,
                  int but) {
    struct box *b = find_box(x, y);
    switch(e) {
    case ce_press:
        switch(but) {
        case WHEEL_UP:   box_inc(b, +1); return;
        case WHEEL_DOWN: box_inc(b, -1); return;
        default: return;
        case 0:
            /* Record state of last press as it determines drag routing. */
            s->b0 = b;
            s->x0 = x;
            s->y0 = y;
        }
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
        if (but != 0) {
            // ZL_LOG("ignoring release %d", but);
            return;
        }
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
    handle_event(ctx, ce, e->xbutton.x, WINDOW_HEIGHT - e->xbutton.y, but);
}


/**** ZL objects ****/

void zl_glx_2d_display(zl_glx *x, zl_xwindow_p xwin,
                       void (*draw)(void*,int,int), void *ctx);
int main(void) {

    init_boxes();

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
