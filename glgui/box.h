#ifndef _BOX_H_
#define _BOX_H_

/* A box is a region of the screen that receives mouse/keyboard events
   and presents graphical data. */

struct box;
struct box_control;

/* Until view/controller part of MVC actually starts to make sense,
   view and controller are combined in one object: there is too much
   information/context sharing going on. */

typedef void (*box_drag_update_t) (struct box *b, struct box_control *bc, int x0, int y0, int dx, int dy);
typedef void (*box_drag_commit_t) (struct box *b, struct box_control *bc);
typedef void (*box_draw_t)        (struct box *b, struct box_control *bc);

/* Abstract the method list in a macro.  This makes it easy to not
   forget to add implementations when the list changes.  See
   implementation. */
#define BOX_METHOD_LIST(m)                      \
    m(drag_update)                              \
    m(drag_commit)                              \
    m(draw)

struct box_class {
#define BOX_CLASS_MEMBER(m) box_##m##_t m;
BOX_METHOD_LIST(BOX_CLASS_MEMBER)
};



struct box {
    /* View */
    const struct box_class *class;
    const char * name;
    int x,y,w,h;
};


/* Box controller. */

struct box_control {
    struct box **boxes; // all boxes
    struct box *b0;     // box of last click
    int x0;             // coords ..
    int y0;
    struct box *b1;     // box of last focus

    /* Random global stuff: FIXME */
    int knob_texture_disk;
    int knob_texture_dial;
    int knob_texture_scale;
};
enum control_event {
    ce_press,
    ce_motion,
    ce_release,
};


/* VARIABLE: a simple variable model capable of representing an
   in-progress edit as v+dv  */
typedef float value;
struct variable {
    value v;
    value dv;
};


/* SLIDER == a box view/controller connected to a variable model. */
struct slider {
    struct box box;
    struct variable *var;
};


/* KNOB == a box view/controller connected to a variable model. */
struct knob {
    struct box box;
    struct variable *var;
};


/* Global controller: called from windowing system. */
void box_control_init(struct box_control *bc, int w, int h);
void box_control_handle_event(struct box_control *bc,
                              enum control_event e, int x, int y,
                              int but);
void box_control_draw_view(void *ctx, int w, int h);

#endif
