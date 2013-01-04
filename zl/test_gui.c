#include "xwindow.h"
#include "glx.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>
#include <zl/config.h>
#include <sys/time.h>  // gettimeofday()
#include <math.h>

#include "segment.h"

/* Bitmap renderer */
#define RENDER_OVERSAMPLE_X 8
#define RENDER_OVERSAMPLE_Y 8

/* 7-segment size */
#define S_T  1 // thickness
#define S_W  3 // horizontal segment width
#define S_H  2 // vertical segment height

#define WINDOW_WIDTH  512
#define WINDOW_HEIGHT 256

#define SLIDER_NX 8
#define SLIDER_NY 2

#define SLIDER_SCALE_PIXELS 200

#define SLIDER_PIXELS 5
#define SLIDER_BORDER 10
#define SLIDER_MARGIN 0

#define KNOB_SCALE_PIXELS 200
#define KNOB_TEXTURE_DIM 64
#define KNOB_DIAL_WIDTH 20

/* [0-1] float seems safest bet */
#define CLIP_LO 0.0f
#define CLIP_HI 1.0f
typedef float value;

#define CLIP clip
static inline value clip(value v) {
    return v > CLIP_HI ? CLIP_HI :
          (v < CLIP_LO ? CLIP_LO : v);
}

typedef unsigned char u8;

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
struct box_control;

typedef void (*set_drag_t) (struct box *b, struct box_control *bc, int x0, int y0, int dx, int dy);
typedef void (*commit_t)   (struct box *b, struct box_control *bc);
typedef void (*draw_t)     (struct box *b, struct box_control *bc);

/* Abstract the method list in a macro.  This makes it easy to not
   forget to add implementations when the list changes. */
#define METHOD_LIST(m) \
    m(set_drag) \
    m(commit) \
    m(draw)

struct box_class {
#define BOX_CLASS_MEMBER(m) m##_t m;
METHOD_LIST(BOX_CLASS_MEMBER)
};



struct box {

    /* View */
    const struct box_class *class;
    const char * name;
    int x,y,w,h;

    /* Control */
    bool focus;
};


/* Box controller. */

struct box_control {
    struct box **boxes; // all boxes
    struct box *b0;     // box of last click
    int x0;             // coords ..
    int y0;
    struct box *b1;     // box of last focus

    /* Random global stuff. */
    GLuint knob_texture_disk;
    GLuint knob_texture_dial;
};
enum control_event {
    ce_press,
    ce_motion,
    ce_release,
};

bool box_control_focus(struct box_control *bc, struct box *b) {
    return bc->b1 == b;
}


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

static void gl_rect_tex(GLuint tex, int x0, int y0, int x1, int y1) {
    // ZL_LOG("gl_rect(%d,%d,%d,%d)", x0, y0, x1, y1);
    glEnable(GL_TEXTURE_2D);
    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE);
    glBindTexture(GL_TEXTURE_2D, tex);

    glBegin(GL_QUADS);
        glTexCoord2f (0,0); glVertex2i(x0, y0);
        glTexCoord2f (1,0); glVertex2i(x1, y0);
        glTexCoord2f (1,1); glVertex2i(x1, y1);
        glTexCoord2f (0,1); glVertex2i(x0, y1);
    glEnd();

    glDisable (GL_BLEND);
    glDisable (GL_TEXTURE_2D);
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
struct segment_pen {
    int w,h,x,y;
};
static void segment_pen_draw(struct segment_pen *p, bool on) {
    if (segment_color(on))
        gl_rect_w(p->x,p->y,p->w,p->h);
    p->y += S_T+S_H;
};
static void segment_draw(unsigned char s, int x0) {

    /* Horizontal bars */
    struct segment_pen p;
    p.x = S_T+x0;
    p.y = 0;
    p.w = S_W;
    p.h = S_T;
    segment_pen_draw(&p, s&S_D);
    segment_pen_draw(&p, s&S_G);
    segment_pen_draw(&p, s&S_A);

    /* Left bars */
    p.x = x0;
    p.y = S_T;
    p.w = S_T;
    p.h = S_H;
    segment_pen_draw(&p, s&S_E);
    segment_pen_draw(&p, s&S_F);

    /* Right bars */
    p.x = S_T+S_W+x0;
    p.y = S_T;
    p.w = S_T;
    p.h = S_H;
    segment_pen_draw(&p, s&S_C);
    segment_pen_draw(&p, s&S_B);
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
    value v;
    value dv;
};
value variable_get(struct variable *var) {
    return var->v + var->dv;
}
void variable_set_delta(struct variable *var, value dv) {
    /* Keep v+dv in proper range. */
    value v = clip(var->v + dv);
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
void slider_set_drag(struct slider *s,
                     struct box_control *bc,
                     int x0, int y0, int dx, int dy) {
    if (s->var) variable_set_delta(s->var, ((value)dy) / SLIDER_SCALE_PIXELS);
}
void slider_commit(struct slider *s,
                   struct box_control *bc) {
    if (s->var) variable_commit(s->var);
}
void slider_draw(struct slider *s,
                 struct box_control *bc) {

    /* Vertical displacement in pixels. */
    value val = variable_get(s->var);
    int v = s->box.h * val;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

        glTranslatef(s->box.x, s->box.y, 0);

        /* Background square */
        if (box_control_focus(bc, &s->box)) {
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
            segment_draw_number(val * 100, 3);
        }


    glPopMatrix();
}
struct box_class slider_class = {
#define BOX_SLIDER_MEMBER(m) .m = (m##_t)(slider_##m),
METHOD_LIST(BOX_SLIDER_MEMBER)
};

/* KNOB == a box connected to a variable model. */
struct knob {
    struct box box;
    struct variable *var;
};
void knob_set_drag(struct knob *s,
                   struct box_control *bc,
                   int x0, int y0, int dx, int dy) {
    if (s->var) variable_set_delta(s->var,  ((value)dy) / KNOB_SCALE_PIXELS);
}
void knob_commit(struct knob *s,
                   struct box_control *bc) {
    if (s->var) variable_commit(s->var);
}


/* Dumb no-nonsense declarative (relational) anti-aliased renderer.
   Creates a bitmap image from a coordinate member function. */
typedef bool (*render_member_fn)(double x, double y);
static void render_spec(u8 *data, int max_x, int max_y, render_member_fn member, bool accu) {
    if (!accu) bzero(data, max_x * max_y);

    /* Coordinates passed to member function are square [-1,1] x [-1,1] */
    double scale_x = 2.0 / ((double)(RENDER_OVERSAMPLE_X * max_x));
    double scale_y = 2.0 / ((double)(RENDER_OVERSAMPLE_Y * max_y));

    u8 *d = data;
    for(int y = 0; y < max_y; y++) {
    for(int x = 0; x < max_x; x++) {
        int acc = 0;
        for (int ys = 0; ys < RENDER_OVERSAMPLE_Y; ys++) {
        for (int xs = 0; xs < RENDER_OVERSAMPLE_X; xs++) {
            int xi = (xs + (x * RENDER_OVERSAMPLE_X));
            int yi = (ys + (y * RENDER_OVERSAMPLE_Y));
            double xf = scale_x * ((float)xi) - 1;
            double yf = scale_y * ((float)yi) - 1;
            acc += member(xf,yf) ? 1 : 0;
        }}
        /* Store pixel with saturation. */
        int out_levels = 1 << (8 * sizeof(*data));
        int acc_levels = (RENDER_OVERSAMPLE_X * RENDER_OVERSAMPLE_Y);
        if      (acc_levels > out_levels) { acc /= (acc_levels / out_levels); }
        else if (acc_levels < out_levels) { acc *= (out_levels / acc_levels); }

        acc += *d;
        if (acc > 0xFF) acc = 0xFF;
        *d++ = acc;
    }}
}

/* Render form specification to raw bitmap and convert to OpenGL texture. */
static GLuint render_texture(render_member_fn member) {
    int w = KNOB_TEXTURE_DIM;
    int h = KNOB_TEXTURE_DIM;
    GLuint tex;
    glGenTextures(1, &tex);
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    u8 *data = malloc(w*h);
    render_spec(data, w, h, member, false);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, w, h, 0, GL_ALPHA, GL_UNSIGNED_BYTE, data);
    free(data);
    return tex;
}

/* Form specifications. */
static bool spec_disk(double x, double y) { return (x*x) + (y*y) < 1.0; }
static bool spec_dial(double x, double y) { return fabs(x) < 0.1; }


/* Generate textures from specs. */
static GLuint knob_texture_disk(void) { return render_texture(spec_disk); }
static GLuint knob_texture_dial(void) { return render_texture(spec_dial); }


void knob_draw(struct knob *s,
               struct box_control *bc) {
    /* Vertical displacement in pixels. */
    value val = variable_get(s->var);
    float angle = (150 - val*300);

    /* Enable texture */
    if (!bc->knob_texture_disk) {
        bc->knob_texture_disk = knob_texture_disk();
        bc->knob_texture_dial = knob_texture_dial();
        ZL_LOG("knob_texture_disk = %d", bc->knob_texture_disk);
        ZL_LOG("knob_texture_dial = %d", bc->knob_texture_dial);
    }

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

        glTranslatef(s->box.x, s->box.y, 0);

        /* Background square */
        if (box_control_focus(bc, &s->box)) {
            glColor3f(0.1,0.1,0.1);
            // int v1 = CLIP(v+20);
            // glColor3ub(v1,v,v);
        }
        else {
            glColor3f(0,0,0);
            // glColor3ub(v,v,v);
        }
        gl_rect(0,0,s->box.w,s->box.h);

        /* Numbers */
        if (1) {
            segment_draw_number(val * 100, 3);
        }

        /* Move to the center of the box. */
        glTranslatef(s->box.w/2, s->box.h/2, 0);
        int r = (s->box.w > s->box.h ? s->box.h : s->box.w) / 3;

        /* Scale ticks */
        if (0) {
            glColor4f(1,1,1,1);
            glPushMatrix();
            glRotatef(150,0,0,1);
            int i;
            float a;
            for (i=0,a=-150; i<11; i++,a+=30) {
                int yoff = r;
                gl_rect_tex(bc->knob_texture_dial, -r,yoff-r,+r,yoff+r);
                glRotatef(-30,0,0,1);
            }
            glPopMatrix();
        }

        /* Use the value to rotate. */
        glRotatef(angle,0,0,1);

        glColor4f(.7,.2,.1,1);
        gl_rect_tex(bc->knob_texture_disk, -r,-r,+r,+r);

        //glColor4f(1,1,1,1);
        //gl_rect_tex(bc->knob_texture_dial, -r,-r,+r,+r);

        glTranslatef(0,r/2,0);
        glScalef(.5,.5,1);
        glColor4f(1,1,1,1);
        gl_rect_tex(bc->knob_texture_dial, -r,-r,+r,+r);
        

    glPopMatrix();

}
struct box_class knob_class = {
#define BOX_KNOB_MEMBER(m) .m = (m##_t)(knob_##m),
METHOD_LIST(BOX_KNOB_MEMBER)
};




/* GUI structure: non-hierarchical to keep it simple.
   - global object
   - collection (array) of leaf objects */
#define BOX_FOR(p,a) for(p=&a[0]; *p; p++)  // NULL terminated list of pointers

static void box_control_init(struct box_control *bc) {
    bzero(bc, sizeof(*bc));

    int nx = SLIDER_NX;
    int ny = SLIDER_NY;
    int DW = WINDOW_WIDTH / nx;
    int DH = WINDOW_HEIGHT / ny;
    int x,y;
    int i = 0;

    struct variable *var[nx];
    for (x = 0; x < nx; x++) {
        var[x] = calloc(1, sizeof(var[x]));
        var[x]->v = 0.5;
    }
    bc->boxes = calloc(1 + (nx * ny), sizeof(void *));
    for (y = 0; y < ny; y++) {
        for (x = 0; x < nx; x++) {
            struct slider *s = calloc(1, sizeof(*s));
            s->box.name = "?";
            s->box.x = x * DW + SLIDER_MARGIN;
            s->box.y = y * DH + SLIDER_MARGIN;
            s->box.w = DW - 2*SLIDER_MARGIN;
            s->box.h = DH - 2*SLIDER_MARGIN;
            s->box.class = (y > 0) ? &knob_class : &slider_class;
            s->var = var[x];
            bc->boxes[i++] = &(s->box);
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
struct box *box_control_find_box(struct box_control *bc, int x, int y) {
    struct box **b;
    BOX_FOR(b, bc->boxes) {
        if (in_box(*b, x, y)) return *b;
    }
    return NULL;
}
int box_control_box_index(struct box_control *bc, struct box *_b) {
    struct box **b;
    BOX_FOR(b, bc->boxes) {
        if (*b == _b)
            return b - bc->boxes;
    }
    return -1;
}
void box_control_draw_view(void *ctx, int w, int h) {
    struct box_control *bc = ctx;
    struct box **b;
    glColor3f(0,0,0);
    gl_rect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT);
    BOX_FOR(b, bc->boxes) { (*b)->class->draw(*b, bc); }
}


/**** Abstract event handler */
void box_control_update_focus(struct box_control *bc, struct box *b) {
    if (bc->b1) {
        bc->b1->focus = false;
    }
    if (b) {
        b->focus = true;
    }
    bc->b1 = b;
}

static void box_inc(struct box *b, struct box_control *bc, int inc) {
    if (b) {
        b->class->set_drag(b, bc, 0, 0, 0, inc);
        b->class->commit(b, bc);
    }
}

void box_control_handle_event(struct box_control *bc,
                              enum control_event e, int x, int y,
                              int but) {
    struct box *b = box_control_find_box(bc, x, y);
    switch(e) {
    case ce_press:
        switch(but) {
        case WHEEL_UP:   box_inc(b, bc, +1); return;
        case WHEEL_DOWN: box_inc(b, bc, -1); return;
        default: return;
        case 0:
            /* Record state of last press as it determines drag routing. */
            bc->b0 = b;
            bc->x0 = x;
            bc->y0 = y;
        }
    case ce_motion:
        if (bc->b0) {
            int dx = x - bc->x0;
            int dy = y - bc->y0;
            // ZL_LOG("drag %s (%d, %d)", bc->b0->name, dx, dy);
            bc->b0->class->set_drag(bc->b0, bc, bc->x0, bc->y0, dx, dy);
        }
        else {
            box_control_update_focus(bc, b);
            // ZL_LOG("motion (%d,%d) %s", x, y, b ? b->name : "<none>");
        }
        break;
    case ce_release:
        if (but != 0) {
            // ZL_LOG("ignoring release %d", but);
            return;
        }
        /* Commit delta */
        if (bc->b0) {
            bc->b0->class->commit(bc->b0, bc);
            bc->b0 = NULL;
        }
        else {
            /* Shouldn't happen. */
            ZL_LOG("spurious release");
        }
        box_control_update_focus(bc, b);
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
    box_control_handle_event(ctx, ce, e->xbutton.x,
                             WINDOW_HEIGHT - e->xbutton.y, but);
}


/**** ZL objects ****/

void zl_glx_2d_display(zl_glx *x, zl_xwindow_p xwin,
                       void (*draw)(void*,int,int), void *ctx);
int main(void) {

    struct box_control box_control;
    box_control_init(&box_control);

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
