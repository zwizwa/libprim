#!/usr/bin/python3

from libprim import *

v4l = zl_v4l_new(0)
zl_v4l_open(v4l, "/dev/video0", 1)
bs = bytes_buffer_new(320*240*2)
zl_v4l_next_bytes(v4l, bs, 1)


# print(v4l)
print(bs)
