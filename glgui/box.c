

#include <stdlib.h>
#include <string.h>

#include <GL/gl.h>  // Just OpenGL (no GLX/GLU)

#include <glgui/box.h>
#include <glgui/render_spec.h>
#include <glgui/segment.h>

#include <zl/config.h>

#include <leaf/leaf.h>

/* 7-segment size */
#define S_T  1 // thickness
#define S_W  3 // horizontal segment width
#define S_H  2 // vertical segment height

#define SLIDER_SCALE_PIXELS 200

#define SLIDER_PIXELS 5


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
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
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


/* BOX: shared drawing code */
static void box_draw_background(struct box *b, struct box_control *bc) {
    if (1) {
        if (box_control_has_focus(bc, b)) {
            glColor4f(0.1,0.1,0.1,1);
            glEnable (GL_BLEND);
            glBlendFunc (GL_SRC_ALPHA, GL_ONE);
            gl_rect(0,0,b->w,b->h);
            glDisable (GL_BLEND);
        }
    }
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

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

        glTranslatef(s->box.x, s->box.y, 0);

        /* Background square */
        box_draw_background(&s->box, bc);

        /* Tick marks */
        glColor4f(1,1,1,1);
        gl_rect_tex(bc->texture_slider_ticks,
                    0,0,s->box.w,s->box.h);

        /* Numbers */
        if (1) {
            segment_draw_number(val * 100, 3);
        }

        /* Move to center */
        glTranslatef(s->box.w/2, s->box.h/2, 0);

        /* Slider bar */
        if (1) {
            float v = val - 0.5; // [-0.5 - 0.5]
            v *= s->box.h * (1.0 - SLIDER_BORDER); // tick rendering param
            glColor3f(.7,.7,.7);
            float w = s->box.w/4;
            float h = 4;
            gl_rect(-w, -h + v,
                     w,  h + v);
        }



    glPopMatrix();
}
/* Gather methods in class data structure. */
struct box_class box_slider_class = {
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
static GLuint render_texture(render_spec_fn spec, int w, int h) {
    double aspect = ((double)w) / ((double)h);
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
    render_spec(data, w, h, aspect, spec, false);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, w, h, 0, GL_ALPHA, GL_UNSIGNED_BYTE, data);
    free(data);
    return tex;
}


static void knob_draw(struct knob *s,
                      struct box_control *bc) {
    /* Vertical displacement in pixels. */
    value val = variable_get(s->var);
    float angle = (150 - val*300);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

        glTranslatef(s->box.x, s->box.y, 0);

        /* Background square */
        box_draw_background(&s->box, bc);

        /* Numbers */
        if (1) {
            segment_draw_number(val * 100, 3);
        }

        /* Move to the center of the box. */
        glTranslatef(s->box.w/2, s->box.h/2, 0);
        float r = (s->box.w > s->box.h ? s->box.h : s->box.w) / 3;

        /* Angle scale */
        if (1) {
            float r2 = s->box.w / 2;
            glColor4f(1,1,1,1);
            gl_rect_tex(bc->texture_knob_ticks, -r2,-r2,+r2,+r2);
        }

        glRotatef(angle,0,0,1);

        /* Draw disk */
        if (1) {
            glColor4f(.2,.3,.6,1);
            gl_rect_tex(bc->texture_knob_disk, -r,-r,+r,+r);

            float r0 = r * 0.9;
            glColor4f(.3,.4,.7,1);
            gl_rect_tex(bc->texture_knob_disk, -r0,-r0,+r0,+r0);

        }
        /* Draw dial */
        if (1) {
            glTranslatef(0,r/2,0);
            glScalef(.5,.5,1);
            glColor4f(1,1,1,1);
            gl_rect_tex(bc->texture_knob_notch, -r,-r,+r,+r);
        }

    glPopMatrix();

}
/* Gather methods in class data structure. */
struct box_class box_knob_class = {
#define BOX_KNOB_MEMBER(m) .m = (box_##m##_t)(knob_##m),
BOX_METHOD_LIST(BOX_KNOB_MEMBER)
};




/* GUI structure: non-hierarchical to keep it simple.
   - global object
   - collection (array) of leaf objects */
#define BOX_FOR(p,a) for(p=&a[0]; *p; p++)  // NULL terminated list of pointers



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

/* Fixme: it would be good to limit this to only a particular
   rectangle to allow for incremental updates on different composition
   engines. */

void box_control_draw_view(void *ctx, int w, int h) {
    struct box_control *bc = ctx;
    struct box **b;

    /* Create textures.  This needs to be done with the GL context
       active, so just do it the first time the global draw() function
       is called. */
    if (!bc->texture_knob_disk) {
        ZL_LOG("generating textures");
        bc->texture_knob_disk     = render_texture(spec_knob_disk,  64, 64);
        bc->texture_knob_notch    = render_texture(spec_knob_notch, 32, 32);
        bc->texture_knob_ticks    = render_texture(spec_knob_ticks, 64 ,64);
        bc->texture_slider_ticks  = render_texture(spec_slider_ticks, 64, 128);
    }

    glColor3f(0,0,0);
    gl_rect(0, 0, w, h);

    /* Draw boxes in no particular order; they should not overlap. */
    BOX_FOR(b, bc->boxes) {
        if (*b != bc->box_focus) {
            (*b)->class->draw(*b, bc);
        }
    }
    /* Always draw focused box on top.  This is to fix temporary
       overlap during GUI editing. */
    if (bc->box_focus) {
        bc->box_focus->class->draw(bc->box_focus, bc);
    }
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

static void box_edit_move(struct box *b, struct box_control *bc,
                          int x0, int y0, int dx, int dy) {
    b->x = x0+dx - b->w/2;
    b->y = y0+dy - b->h/2;
}

void box_control_handle_event(struct box_control *bc,
                              enum control_event e, int x, int y,
                              enum button_event but) {
    switch(e) {
    case ce_press:
        /* We don't do multiple button presses. */
        if (bc->current_button != button_none) return;

        switch(but) {
        case button_wheel_up:   box_inc(box_control_find_box(bc, x, y), bc, +1); return;
        case button_wheel_down: box_inc(box_control_find_box(bc, x, y), bc, -1); return;
        default: break;
        }

        /* Record state of last press as it determines drag routing. */
        bc->box_edit = box_control_find_box(bc, x, y);
        box_control_update_focus(bc, bc->box_edit);
        bc->x = x;
        bc->y = y;
        bc->current_button = but;
        break;

    case ce_motion:
        if (bc->box_edit) {
            int dx = x - bc->x;
            int dy = y - bc->y;
            switch(bc->current_button) {
            case button_left:
                /* Normal GUI drag operation: parameter adjust. */
                bc->box_edit->class->drag_update(bc->box_edit, bc, bc->x, bc->y, dx, dy);
                break;
                /* GUI editing. */
            case button_right:
                box_edit_move(bc->box_edit, bc, bc->x, bc->y, dx, dy);
                break;
            default:
                break;
            }
        }
        else {
            /* No edit in progress, allow change of focus. */
            box_control_update_focus(bc, box_control_find_box(bc, x, y));
        }
        break;
    case ce_release:
        /* If it's not the same button as the one that's currently
           down, ignore it */
        if (bc->current_button != but) return;

        /* Commit delta */
        if (bc->box_edit) {
            switch(bc->current_button) {
            case button_left:
                bc->box_edit->class->drag_commit(bc->box_edit, bc);
                bc->box_edit = NULL;
                break;
            default:
                break;
            }
        }
        bc->current_button = button_none;

        /* Release might change focus. */
        box_control_update_focus(bc, box_control_find_box(bc, x, y));

        break;
    default:
        break;
    }
}

struct box_control *box_control_new(int w, int h) {
    struct box_control *bc = calloc(1,sizeof(*bc));
    bc->w = w;
    bc->h = h;
    return bc;
}

void box_control_cleanup_opengl(struct box_control *bc) {
    bc->texture_knob_disk = 0;
    bc->texture_knob_notch = 0;
    bc->texture_knob_ticks = 0;
    bc->texture_slider_ticks = 0;
    bc->current_button = button_none;
    bc->box_edit = NULL;
    bc->box_focus = NULL;
}
