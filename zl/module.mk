m_C   := ffmpeg.c glx.c gsl.c sc_wrap.c v4l.c xv.c xwindow.c yuv.c
m_OUT := test.elf
m_LDFLAGS := -lpthread -L/usr/X11R6/lib -lX11 -lXv -lXext -lGL -lGLU