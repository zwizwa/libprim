#ifndef _RENDER_SPEC_H_
#define _RENDER_SPEC_H_

#include <stdbool.h>

/* Bitmap renderer */
#define RENDER_OVERSAMPLE_X 8
#define RENDER_OVERSAMPLE_Y 8


/* Form specifications. */
bool spec_disk(double x, double y);
bool spec_dial(double x, double y);
bool spec_scale(double x, double y);

/* Dumb no-nonsense declarative (relational) anti-aliased renderer.
   Creates a bitmap image from a coordinate member function. */
typedef bool (*render_spec_fn)(double x, double y);
void render_spec(unsigned char *data, int max_x, int max_y, render_spec_fn spec, bool accu);

#endif
