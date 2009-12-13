include ../Makefile.defs

# JNI_CFLAGS = -I/export/home/jdk1.2/include -I/export/home/jdk1.2/include/linux 
JNI_LDFLAGS = 
# JNI_CFLAGS = \
#	-I/usr/lib/jvm/java-1.5.0-sun-1.5.0.17/include \
#	-I/usr/lib/jvm/java-1.5.0-sun-1.5.0.17/include/linux \

SC_CFLAGS = -I../
SC_LDFLAGS = -L../sc -L../ex -L../leaf -lprim_sc -lprim_ex -lprim_leaf

CPPFLAGS += -I/usr/lib/jvm/java-1.5.0-sun-1.5.0.17/include
CPPFLAGS += -I/usr/lib/jvm/java-1.5.0-sun-1.5.0.17/include/linux

all: libscheme.so sc.class main.class j.class tests.class

links:
	ln -s . libprim
	ln -s . zwizwa

gdbtest: all main.class
	CLASSPATH=. LD_LIBRARY_PATH=. gdb --args java main

test: all main.class
	CLASSPATH=. LD_LIBRARY_PATH=. java main



# JAVA_FLAGS = -Xlint:unchecked

%.class: %.java
	javac $(JAVA_FLAGS)$<

sc.h: sc.class
	javah -jni sc

allclean:
	rm -f *~ *.o *.so *.class

%.so: %.c %.d
	$(CC) -fPIC -o $@ -shared -Wl,-soname,$@ -Wall $(CPPFLAGS)  $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) $(JNI_CFLAGS) $(SC_CFLAGS) $< $(SC_LDFLAGS)

%.h_sc_prims: %.c ../sc/sc_prims.ss ../prim-tools.ss
	$(MZSCHEME) ../sc/sc_prims.ss $< >$@

GEN = libscheme.h_sc_prims
include ../Makefile.rules
