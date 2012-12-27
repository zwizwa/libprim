#include "v4l.h"
#include "xwindow.h"
#include "xv.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int main(void) {
    struct zl_v4l v4l;
    zl_v4l_init(&v4l, true);
    zl_v4l_open(&v4l, "/dev/video0", true);
    // zl_v4l_open(&v4l, "/dev/video1");
    unsigned int fourcc, w,h;
    zl_v4l_get_format(&v4l, &fourcc, &w, &h);

    unsigned char *video_data = NULL;

    zl_xdisplay *xd = zl_xdisplay_new(":0");
    zl_xwindow *xw = zl_xwindow_new();
    zl_xv *xv = zl_xv_new();

    zl_xwindow_config(xw, xd);

    zl_xv_open_on_display(xv, xd, 0);


    int count = 1000;
    unsigned char *image_buffer = zl_xv_image_data(xv, xw, w, h);
    while (count > 0) {
        sleep(0);
        zl_v4l_next(&v4l, &video_data);
        if (NULL != video_data) {
            count--;
            memcpy(image_buffer, video_data, w*h*2);
            zl_xdisplay_route_events(xd);
            zl_xv_image_display(xv, xw);
        }
    }

    sleep(1);
    // zl_xdisplay_register_window(xd, xw);
    // struct zl_xwindow *
    zl_xdisplay_unregister_window(xd, xw);
    zl_xwindow_free(xw);
    zl_xv_free(xv);
    zl_xdisplay_free(xd);

    return 0;
}
