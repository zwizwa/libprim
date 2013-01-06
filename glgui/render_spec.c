#include "render_spec.h"
#include <math.h>
#include <string.h>
#include <zl/config.h>


typedef unsigned char u8;

/* Dumb no-nonsense declarative (relational) anti-aliased renderer.
   Creates a bitmap image from a coordinate member function.

   While simple, this approach suffers from the 'square pixel'
   effect. It might help to smooth the output data a bit more, or use
   overlapping pixels in the first place, e.g. implemented using a
   random sampling method with PDF == convolution shape?  */

void render_spec(u8 *data, int max_x, int max_y, double aspect,
                 render_spec_fn spec, bool accu) {
    if (!accu) bzero(data, max_x * max_y);

    /* Coordinates passed to member function are rectangular [-a,a] x [-1,1] */
    double scale_x = 2.0 / ((double)(RENDER_OVERSAMPLE_X * max_x)) * aspect;
    double scale_y = 2.0 / ((double)(RENDER_OVERSAMPLE_Y * max_y));

    u8 *d = data;
    for(int y = 0; y < max_y; y++) {
    for(int x = 0; x < max_x; x++) {
        int acc = 0;
        for (int ys = 0; ys < RENDER_OVERSAMPLE_Y; ys++) {
        for (int xs = 0; xs < RENDER_OVERSAMPLE_X; xs++) {
            int xi = (xs + (x * RENDER_OVERSAMPLE_X));
            int yi = (ys + (y * RENDER_OVERSAMPLE_Y));
            double xf = scale_x * ((float)xi) - aspect;
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


/* Form specifications.

   Since the whole approach isn't particulary fast, it helps if these
   predicates fail early.  I.e. put the coarsest bounding shape checks
   first.
*/


bool spec_knob_disk(double x, double y) {
    return (x*x) + (y*y) < 1.0;
}
bool spec_knob_notch(double x, double y) {
    return (fabs(x) < 0.1)
        && (fabs(y) < 0.9);
}


bool spec_knob_disk_1(double x, double y) {
    return ((x*x) + (y*y) < 1.0)
        && (fabs(x) < 0.9)
        && (fabs(y) < 0.9);
}

/* Positive float modulo */
static double fmod_pos(double x, double m) {
    while (x >= m) x -= m;
    while (x <  0) x += m;
    return x;
}

#if 0
/* Positive/negative float modulo */
static double fmod_pos_neg(double x, double m) {
    double m2 = m/2;
    while (x >= m2) x -= m;
    while (x < -m2) x += m;
    return x;
}
#endif

static double pos_deg(double deg) {
    return fmod_pos(deg, 360);
}


bool spec_knob_ticks(double x, double y) {
    double r = sqrt((x*x) + (y*y));

    /* All marks are inside annulus */
    if (r < SCALE_R_INNER) return false;
    if (r > SCALE_R_OUTER) return false;

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
    int nb_segments = SCALE_TICKS;
    double segm_deg = 300 / nb_segments;
    if (!(pos_deg(deg) < (nb_segments * segm_deg + tick_deg))) return false;

    /* Paint the outer ring */
    if (r > SCALE_R_RING) return true;

    /* Strip off the "real" part of the angle == modulo operation. */
    int segment = pos_deg(deg)/segm_deg;
    deg -= segment * segm_deg;

    /* Only paint the tick width. */
    return pos_deg(deg) < tick_deg;

}




bool spec_slider_ticks(double x, double y) {
    /* Coarse bounding box. */
    if (fabs(y) > (1 - SLIDER_BORDER + SLIDER_THICKNESS/2)) return false;
    if (fabs(x) > SLIDER_WITH_COARSE) return false;

    /* Slot line line */
    if (fabs(x) < 0.06) { return fabs(x) > 0.05; }

    /* Translate to tick mark coordinates, compensated for slider thickness. */
    double tick_spacing = (1 - SLIDER_BORDER) / (SLIDER_TICKS/2);
    double mark_frac = 5.0 + (y / tick_spacing);

    /* Separate in coarse/fine tick-relative coordinates. */
    int mark = mark_frac;
    mark_frac -= mark;

    /* Only 0,5,10,... are wider */
    if ((mark % (SLIDER_TICKS / 2) != 0) &&
        (fabs(x) > SLIDER_WITH_FINE)) return false;

    return fabs(mark_frac) < SLIDER_THICKNESS/2;
}
