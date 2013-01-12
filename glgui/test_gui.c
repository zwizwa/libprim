
#include <glgui/glxgui.h>
#include <unistd.h>

#define SLIDER_NX 8
#define SLIDER_NY 2

void box_control_config(struct box_control *bc) {
    int nx = SLIDER_NX;
    int ny = SLIDER_NY;
    int DW = bc->w / nx;
    int DH = bc->h / ny;
    int x,y;
    int i = 0;

    /* Model */
    bc->p = dparam_new(SLIDER_NX);

    /* View/Controller */
    bc->boxes = calloc(1 + (nx * ny), sizeof(void *));
    for (y = 0; y < ny; y++) {
        for (x = 0; x < nx; x++) {
            struct slider *s = calloc(1, sizeof(*s));
            s->box.name = "?";
            s->box.x = x * DW;
            s->box.y = y * DH;
            s->box.w = DW;
            if (y > 0) {
                s->box.h = DH/2;
                s->box.class = &box_knob_class;
            }
            else {
                s->box.h = DH;
                s->box.class = &box_slider_class;
            }
            s->param = x;
            bc->boxes[i++] = &(s->box);
        }
    }
}
int main(void) {
    struct box_control *bc = box_control_new(512,256);
    box_control_config(bc);
    glxgui_open(bc);
    while (1) sleep (1);
}
