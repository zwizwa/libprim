#ifndef _RENDER_SPEC_H_
#define _RENDER_SPEC_H_

#include <stdbool.h>

/* Bitmap renderer */
#define RENDER_OVERSAMPLE_X 8
#define RENDER_OVERSAMPLE_Y 8


/* Form specifications in the form of coordinate predicates. */
bool spec_knob_disk(double x, double y);
bool spec_knob_disk_1(double x, double y);
bool spec_knob_notch(double x, double y);


/* Rotary knob tick dials + outer border.
   View/controller use the following assumptions:
   - standard 300 degree angle span
   - midway points up
   - span cut-out at the bottom
   - clockwize = increment (controller assumption)
*/

#define SCALE_R_OUTER 0.90
#define SCALE_R_RING  0.85
#define SCALE_R_INNER 0.70
#define SCALE_TICKS     10
bool spec_knob_ticks(double x, double y);


/* Vertical slider tick marks.
   View/controller use the following assumptions:
   - midway is center
   - top/bottom limits are at 5% (0.10 in [-1,1] coordinates)
   - aspect = 0.5  ->  x \in [-0.5,0.5]  y \in [-1,1]
*/
#define SLIDER_BORDER      0.10
#define SLIDER_THICKNESS   0.10
#define SLIDER_TICKS       10
#define SLIDER_WITH_COARSE 0.30
#define SLIDER_WITH_FINE   0.20
bool spec_slider_ticks(double x, double y);

/* Create a bitmap image from a coordinate predicate :: (x,y) -> bool */
typedef bool (*render_spec_fn)(double x, double y);
void render_spec(unsigned char *data, int max_x, int max_y, double aspect,
                 render_spec_fn spec, bool accu);

#endif
