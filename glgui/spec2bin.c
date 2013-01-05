#include <glgui/render_spec.h>
#include <stdio.h>

#define DIM 64
unsigned char buf[DIM * DIM];

int main(void) {
    render_spec(buf, DIM, DIM, 1, spec_knob_ticks, false);
    fwrite(buf, 1, sizeof(buf), stdout);
    // convert -size 64x64 -depth 8 GRAY:test.bin test.png
    return 0;
}
