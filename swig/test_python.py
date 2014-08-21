#!/usr/bin/python3

from libprim import *

v4l = zl_v4l_new(0)
zl_v4l_open(v4l, "/dev/video0", 1)
fourcc = zl_v4l_fourcc(v4l)
width  = zl_v4l_width(v4l)
height = zl_v4l_height(v4l)

print("%08x : %d x %d" % (fourcc, width, height))

bs = bytes_buffer_new(width * height * 2)
zl_v4l_next_bytes(v4l, bs, 1)

codec_context = codec_context_new()
codec = codec_new("")
print(codec_context_open(codec_context, codec))
vframe = vframe_new(codec_context)
aframe = aframe_new(codec_context)



# print(v4l)
print(bs)
