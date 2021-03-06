/* libprim/media bindings for Scheme

   This file bridges the Scheme memory / control (exception) model
   with low-level C objects.  The function prototypes are mined to
   produce media.h_sc_prims contating media_table_init + macro defs.

   FIXME: 
     * this file should be autogenerated from the media/.h C objects. 
     * extend leaf objects with error handling mechanism
     * move some operations from scheme-specific to generic leaf.

 */


// #include "scheme.h"
#include <zl/ffmpeg.h>
#include <leaf/bytes.h>
#include <zl/xwindow.h>
#include <zl/xv.h>
#include <zl/glx.h>
#include <leaf/grid.h>
#include <zl/gsl.h>
#include <zl/yuv.h>

/*** X11 ***/

#include <ex/ex.h>
#include <sc/sc.h>

DEF_LEAF(zl_xwindow)
DEF_LEAF(zl_xdisplay)
DEF_LEAF(grid)
DEF_LEAF(grid_proc)

_ sc_make_display(sc *sc, _ ob) {
    return _sc_make_aref(sc, zl_xdisplay_new(CAST(cstring, ob)));
}
_ sc_make_window(sc *sc) {
    return _sc_make_aref(sc, zl_xwindow_new());
}
_ sc_window_config(sc *sc, _ win, _ disp) {
    zl_xwindow_config(CAST(zl_xwindow, win),
                      CAST(zl_xdisplay, disp));
    return VOID;
}

