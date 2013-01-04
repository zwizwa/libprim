#include "render_spec.h"
#include <math.h>
#include <string.h>
#include <zl/config.h>


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
bool spec_disk(double x, double y) {
    return (x*x) + (y*y) < 1.0;
}
bool spec_dial(double x, double y) {
    return (fabs(x) < 0.1)
        && (fabs(y) < 0.9);
}


bool spec_disk_1(double x, double y) {
    return ((x*x) + (y*y) < 1.0)
        && (fabs(x) < 0.8)
        && (fabs(y) < 0.8);
}

static double pos_deg(double deg) {
    while (deg >= 360) deg -= 360;
    while (deg < 0)    deg += 360;
    return deg;
}

/* Rotary knob tick dials + outer border. */

bool spec_scale(double x, double y) {
    double r = sqrt((x*x) + (y*y));

    /* All marks are inside annulus */
    if (r < .70) return false;
    if (r > .90) return false;

    /* Angle from [-180,180] degrees. */
    double deg = atan2(y,x) * (180 / M_PI);

    /* Rotate the coordinate system such that the "0" position at
       7 o'clock is 0 degrees, and angles inrement in clockwise
       direction. */
    double deg_0 = 240;
    deg = deg_0 - deg;

    /* Slight rotation for half of the tick width so we can use
       positive numbers below. */
    double tick_deg = 4;
    deg += tick_deg/2;

    /* Cut out the 2 bottom segments of the scale, except the 2 halves
       of the tick marks that stick out. */
    int nb_segments = 10;
    double segm_deg = 300 / nb_segments;
    if (!(pos_deg(deg) < (nb_segments * segm_deg + tick_deg))) return false;

    /* Paint the outer ring */
    if (r > .85) return true;

    /* Strip off the "real" part of the angle == modulo operation. */
    int segment = pos_deg(deg)/segm_deg;
    deg -= segment * segm_deg;

    /* Only paint the tick width. */
    return pos_deg(deg) < tick_deg;

}
