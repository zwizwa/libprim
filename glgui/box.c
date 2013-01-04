

#include <stdlib.h>
#include <string.h>

#include <GL/gl.h>  // Just OpenGL (no GLX/GLU)

#include <glgui/box.h>
#include <glgui/render_spec.h>
#include <glgui/segment.h>

/* 7-segment size */
#define S_T  1 // thickness
#define S_W  3 // horizontal segment width
#define S_H  2 // vertical segment height
#define SLIDER_NX 8
#define SLIDER_NY 2

#define SLIDER_SCALE_PIXELS 200

#define SLIDER_PIXELS 5
#define SLIDER_BORDER 10
#define SLIDER_MARGIN 0

#define KNOB_SCALE_PIXELS 200
#define KNOB_DIAL_WIDTH 20

/* [0-1] float seems safest bet */
#define CLIP_LO 0.0f
#define CLIP_HI 1.0f

#define CLIP clip
static inline value clip(value v) {
    return v > CLIP_HI ? CLIP_HI :
          (v < CLIP_LO ? CLIP_LO : v);
}



typedef unsigned char u8;

#define WHEEL_UP   3
#define WHEEL_DOWN 4

/* Simplicity is probably more important than efficiency here.

   All GUI zones are squares.  Since the GUI layout doesn't change,
   the event routing could be implemented as a flat list.

   What is interesting here is the difference between an ongoing edit
   (drag = delta) and a committed edit.  This popped out naturally.

*/


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


value variable_get(struct variable *var) {
    return var->v + var->dv;
}
void variable_set_delta(struct variable *var, value dv) {
    /* Keep v+dv in proper range. */
    value v = clip(var->v + dv);
    var->dv = v - var->v;
}
void variable_drag_commit(struct variable *var) {
    var->v += var->dv;
    var->dv = 0;
}


/* SLIDER */

static void slider_drag_update(struct slider *s,
                               struct box_control *bc,
                               int x0, int y0, int dx, int dy) {
    if (s->var) variable_set_delta(s->var, ((value)dy) / SLIDER_SCALE_PIXELS);
}
static void slider_drag_commit(struct slider *s,
                               struct box_control *bc) {
    if (s->var) variable_drag_commit(s->var);
}
static void slider_draw(struct slider *s,
                 struct box_control *bc) {

    /* Vertical displacement in pixels. */
    value val = variable_get(s->var);
    int v = s->box.h * val;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

        glTranslatef(s->box.x, s->box.y, 0);

        /* Background square */
        if (box_control_has_focus(bc, &s->box)) {
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
/* Gather methods in class data structure. */
struct box_class slider_class = {
#define BOX_SLIDER_MEMBER(m) .m = (box_##m##_t)(slider_##m),
BOX_METHOD_LIST(BOX_SLIDER_MEMBER)
};






static void knob_drag_update(struct knob *s,
                             struct box_control *bc,
                             int x0, int y0, int dx, int dy) {
    if (s->var) variable_set_delta(s->var,  ((value)dy) / KNOB_SCALE_PIXELS);
}
static void knob_drag_commit(struct knob *s,
                             struct box_control *bc) {
    if (s->var) variable_drag_commit(s->var);
}



/* Render form specification to raw bitmap and convert to OpenGL texture. */
static GLuint render_texture(render_spec_fn spec, int dim) {
    int w = dim;
    int h = dim;
    GLuint tex;
    glGenTextures(1, &tex);
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); /* Needs OpenGL 1.4 */
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    u8 *data = malloc(w*h);
    render_spec(data, w, h, spec, false);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, w, h, 0, GL_ALPHA, GL_UNSIGNED_BYTE, data);
    free(data);
    return tex;
}


static void knob_draw(struct knob *s,
                      struct box_control *bc) {
    /* Vertical displacement in pixels. */
    value val = variable_get(s->var);
    float angle = (150 - val*300);

    /* Enable texture */
    if (!bc->knob_texture_disk) {
        bc->knob_texture_disk  = render_texture(spec_disk, 64);
        bc->knob_texture_dial  = render_texture(spec_dial, 32);
        bc->knob_texture_scale = render_texture(spec_scale, 64);
    }

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

        glTranslatef(s->box.x, s->box.y, 0);

        /* Background square */
        if (box_control_has_focus(bc, &s->box)) {
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

        /* Angle scale */
        if (1) {
            int r2 = s->box.w / 2;
            glColor4f(1,1,1,1);
            gl_rect_tex(bc->knob_texture_scale, -r2,-r2,+r2,+r2);
        }

        glRotatef(angle,0,0,1);

        /* Draw disk */
        if (1) {
            glColor4f(.7,.2,.1,1);
            gl_rect_tex(bc->knob_texture_disk, -r,-r,+r,+r);
        }
        /* Draw dial */
        if (1) {
            glTranslatef(0,r/2,0);
            glScalef(.5,.5,1);
            glColor4f(1,1,1,1);
            gl_rect_tex(bc->knob_texture_dial, -r,-r,+r,+r);
        }

    glPopMatrix();

}
/* Gather methods in class data structure. */
struct box_class knob_class = {
#define BOX_KNOB_MEMBER(m) .m = (box_##m##_t)(knob_##m),
BOX_METHOD_LIST(BOX_KNOB_MEMBER)
};




/* GUI structure: non-hierarchical to keep it simple.
   - global object
   - collection (array) of leaf objects */
#define BOX_FOR(p,a) for(p=&a[0]; *p; p++)  // NULL terminated list of pointers

void box_control_init(struct box_control *bc, int w, int h) {
    bzero(bc, sizeof(*bc));

    int nx = SLIDER_NX;
    int ny = SLIDER_NY;
    int DW = w / nx;
    int DH = h / ny;
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

void box_control_draw_view(void *ctx, int w, int h) {
    struct box_control *bc = ctx;
    struct box **b;
    glColor3f(0,0,0);
    gl_rect(0, 0, w, h);
    BOX_FOR(b, bc->boxes) { (*b)->class->draw(*b, bc); }
}


/**** Abstract event handler */
static void box_control_update_focus(struct box_control *bc, struct box *b) {
    bc->box_focus = b;
}
bool box_control_has_focus(struct box_control *bc, struct box *b) {
    return bc->box_focus == b;
}


static void box_inc(struct box *b, struct box_control *bc, int inc) {
    if (b) {
        b->class->drag_update(b, bc, 0, 0, 0, inc);
        b->class->drag_commit(b, bc);
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
            bc->box_edit = b;
            bc->x = x;
            bc->y = y;
        }
    case ce_motion:
        if (bc->box_edit) {
            int dx = x - bc->x;
            int dy = y - bc->y;
            bc->box_edit->class->drag_update(bc->box_edit, bc, bc->x, bc->y, dx, dy);
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
        if (bc->box_edit) {
            bc->box_edit->class->drag_commit(bc->box_edit, bc);
            bc->box_edit = NULL;
        }
        else {
            /* Shouldn't happen. */
            // ZL_LOG("spurious release");
        }
        box_control_update_focus(bc, b);
        break;
    default:
        break;
    }
}
