# Library
m_C   := render_spec.c box.c

# Test & util code
m_ELF := spec2bin.elf test_gui.elf
m_LDFLAGS := -lpthread -L/usr/X11R6/lib -lX11 -lXv -lXext -lGL -lGLU



