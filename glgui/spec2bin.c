#include <glgui/render_spec.h>
#include <stdio.h>

#define DIM 64
unsigned char buf[DIM * DIM];

int main(void) {
    render_spec(buf, DIM, DIM, spec_disk, false);
    fwrite(buf, 1, sizeof(buf), stdout);
    return 0;
}
