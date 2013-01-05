#ifndef _BOX_H_
#define _BOX_H_

#include <stdbool.h>

/* A box is a region of the screen that receives mouse/keyboard events
   and presents graphical data. */

struct box;
struct box_control;
struct box_class;

/* Box is subclassed.  Since we have a very simple hierarchy, this is
   all just C and a bunch of red tape macros.  Each object points to a
   class, which is a (const) struct of member functions. */
struct box {
    const struct box_class *class;
    const char * name;
    int x,y,w,h;
};

/* Until view/controller part of MVC actually starts to make sense,
   view and controller are combined in one object: there is too much
   information/context sharing going on. */
typedef void (*box_drag_update_t) (struct box *b, struct box_control *bc, int x0, int y0, int dx, int dy);
typedef void (*box_drag_commit_t) (struct box *b, struct box_control *bc);
typedef void (*box_draw_t)        (struct box *b, struct box_control *bc);

/* Abstract the method list in a macro.  This abstracts some
   compile-time red tape.  See also box.c */
#define BOX_METHOD_LIST(m)                      \
    m(drag_update)                              \
    m(drag_commit)                              \
    m(draw)

struct box_class {
#define BOX_CLASS_MEMBER(m) box_##m##_t m;
BOX_METHOD_LIST(BOX_CLASS_MEMBER)
};





/* Box controller. */

struct box_control {
    struct box **boxes;    // all boxes
    struct box *box_edit;  // box currently being edited (mouse drag)
    int x, y;              // coords of last click
    struct box *box_focus; // box of last focus

    /* Global view data. */
    int texture_knob_disk;
    int texture_knob_notch;
    int texture_knob_ticks;
    int texture_slider_ticks;
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

/* Box -> box controller queries */
bool box_control_has_focus(struct box_control *bc, struct box *b);

#endif
