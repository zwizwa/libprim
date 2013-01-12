#ifndef _BOX_H_
#define _BOX_H_

#include <stdbool.h>
#include <leaf/queue.h>
#include <glgui/dparam.h>

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
typedef void (*box_edit_click_t)  (struct box *b, struct box_control *bc, int x0, int y0);
typedef void (*box_edit_drag_t)   (struct box *b, struct box_control *bc, int x0, int y0, int dx, int dy);
typedef void (*box_draw_t)        (struct box *b, struct box_control *bc);

/* Abstract the method list in a macro.  This abstracts some
   compile-time red tape.  See also box.c */
#define BOX_METHOD_LIST(m)                      \
    m(edit_click)                               \
    m(edit_drag)                                \
    m(draw)

struct box_class {
#define BOX_CLASS_MEMBER(m) box_##m##_t m;
BOX_METHOD_LIST(BOX_CLASS_MEMBER)
};

extern struct box_class box_knob_class;
extern struct box_class box_slider_class;

/* Box controller. */
enum control_event {
    /* GUI operation events. */
    ce_press,
    ce_motion,
    ce_release,
};
enum button_event {
    button_none       = 0,
    button_left       = 1,
    button_middle     = 2,
    button_right      = 3,
    button_wheel_up   = 4,
    button_wheel_down = 5,

    /* For editing: keyboard behaves as mouse buttons. */
    button_move,

};

struct box_control {
    /* Layout */
    int w, h;              // window dimensions
    struct box **boxes;    // all boxes

    /* Current control state */
    struct box *box_edit;  // box currently being edited (mouse drag)
    int x, y;              // coords of last click
    value v0;              // param value at last click
    struct box *box_focus; // box of last focus
    enum button_event current_button;

    /* Control pipes to real model state. */
    struct dparam *p;

    /* Global view data. */
    int texture_knob_disk;
    int texture_knob_notch;
    int texture_knob_ticks;
    int texture_slider_ticks;
};



/* SLIDER == a box view/controller connected to a variable model. */
struct slider {
    struct box box;
    int param;
};


/* KNOB == a box view/controller connected to a variable model. */
struct knob {
    struct box box;
    int param;
};


/* Global controller: called from windowing system. */
struct box_control *box_control_new(int w, int h);

/* Clean up OpenGL state before closing window. */
void box_control_cleanup_opengl(struct box_control *bc);

void box_control_handle_event(struct box_control *bc,
                              enum control_event e, int x, int y,
                              enum button_event but);
void box_control_draw_view(void *ctx, int w, int h);

/* Box -> box controller queries */
bool box_control_has_focus(struct box_control *bc, struct box *b);


/* CORE <-> GUI messages */


/* CORE <-> GUI messages */
enum message_id {
    message_id_none = 0,
    message_id_params,
    message_id_waveform,
};
struct message {
    enum message_id id;
}  __attribute__((packed));
struct message_array {
    struct message msg;
    int nb_el;
    float el[0];
}  __attribute__((packed));


// CORE -> GUI
void box_control_update_gui(struct box_control *bc,
                            int nb_p, const float *p);


#endif
