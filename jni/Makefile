
JNI_CFLAGS = -I/export/home/jdk1.2/include -I/export/home/jdk1.2/include/linux 
JNI_LDFLAGS = 

all: testnative.class libnativelib.so reflect.class

# JAVA_FLAGS = -Xlint:unchecked

%.class: %.java
	javac $(JAVA_FLAGS)$<

testnative.class: testnative.java
	javac testnative.java

libnativelib.so: nativelib.c testnative.h
	gcc -o libnativelib.so -shared -Wl,-soname,libnativelib.so  \
		$(JNI_CFLAGS) nativelib.c  -static -lc

testnative.h: testnative.class
	javah -jni testnative

clean:
	rm -f *~ *.o *.so *.class testnative.h

test: all
	LD_LIBRARY_PATH=`pwd` java testnative

