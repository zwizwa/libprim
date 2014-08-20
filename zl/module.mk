m_C   := ffmpeg.c glx.c gsl.c sc_wrap.c v4l.c xv.c xwindow.c yuv.c layout2D.c
m_ELF := # test_v4l_xv.elf
m_LDFLAGS := \
	-lpthread \
	-L/usr/X11R6/lib -lX11 -lXv -lXext -lGL -lGLU \
	-lavcodec -lavformat
