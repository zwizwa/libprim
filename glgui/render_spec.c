#include "render_spec.h"
#include <math.h>
#include <string.h>


typedef unsigned char u8;

/* Dumb no-nonsense declarative (relational) anti-aliased renderer.
   Creates a bitmap image from a coordinate member function. */
void render_spec(u8 *data, int max_x, int max_y, render_spec_fn spec, bool accu) {
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
            acc += spec(xf,yf) ? 1 : 0;
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


/* Form specifications. */
bool spec_disk(double x, double y) { return (x*x) + (y*y) < 1.0; }
bool spec_dial(double x, double y) { return fabs(x) < 0.1; }


bool spec_disk_1(double x, double y) {
    return ((x*x) + (y*y) < 1.0)
        && (fabs(x) < 0.8)
        && (fabs(y) < 0.8);
}

static void rotate_deg(double angle, double *x, double *y) {
    double ar = angle * (M_PI * 180.0);
    double c = cos(ar);
    double s = sin(ar);
    double _x =  c * (*x) - s * (*y);
    double _y =  s * (*x) + c * (*y);
    *x = _x;
    *y = _y;
}

bool spec_dial_scale_angle(double angle, double x, double y) {
    rotate_deg(angle, &x, &y);
    return (fabs(x) < 0.1) && (y > 0.5);
}
bool spec_dial_scale(double x, double y) {
    bool rv;
    double angle = 150;
    for (int i = 0; i < 11; i++) {
        rv = rv && spec_dial_scale_angle(angle, x, y);
        angle += 30;
    }
    return rv;
}
