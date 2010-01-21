include Makefile.defs

DIRS = leaf $(MEDIA) $(EX) $(SC) $(PF) $(JNI)

all: 
	for dir in $(DIRS); do make -C $$dir; done

rec_clean:
	rm -f *~
	for dir in $(DIRS); do make -C $$dir clean; done

# This doesn't trigger .h code generation.
JUNK=*.o *.d *.d.* *.a *~ */*~ *.class *.so
clean:
	rm -f *~
	cd leaf   ; rm -f $(JUNK)
	cd ex     ; rm -f $(JUNK)
	cd sc     ; rm -f $(JUNK) sc
	cd jni    ; rm -f $(JUNK) 
	cd pf     ; rm -f $(JUNK) pf


mrproper:
	for dir in $(DIRS); do make -C $$dir mrproper; done

# Generate C code.  The resulting tree can be built without mzscheme installed.
gen:
	for dir in $(DIRS); do make -C $$dir gen; done

# check code size
strip64:
	make -C . clean
	make -C . "CC = gcc -m64"
	make -C . strip

strip32:
	make -C . clean
	make -C . "CC = gcc -m32"
	make -C . strip
strip:
	find -name '*.o' -exec strip '{}' ';'
	find -name '*.o' -exec ls -l '{}' ';'
	@find -name '*.o' -exec rm  '{}' ';'


install: all
	install -d $(PREFIX)/lib/pkgconfig
	install -m 644 libprim.pc $(PREFIX)/lib/pkgconfig

	install -d $(PREFIX)/share/prim/
	install -m 644 $(SRCDIR)/sc/*.scm $(PREFIX)/share/prim/
	install -m 644 $(SRCDIR)/pf/*.pf $(PREFIX)/share/prim/

	install -d $(PREFIX)/include/prim/ex
	install -d $(PREFIX)/include/prim/leaf
	install -d $(PREFIX)/include/prim/sc
	install -d $(PREFIX)/include/prim/media

	install -m 644 $(SRCDIR)/ex/*.h* $(PREFIX)/include/prim/ex/
	install -m 644 $(SRCDIR)/sc/*.h* $(PREFIX)/include/prim/sc/
	install -m 644 $(SRCDIR)/leaf/*.h* $(PREFIX)/include/prim/leaf/
	install -m 644 $(SRCDIR)/media/*.h* $(PREFIX)/include/prim/media/

	install -m 755 */libprim_*.a $(PREFIX)/lib/

	install -d $(PREFIX)/bin
	install -m 755 sc/sc $(PREFIX)/bin
	install -m 755 pf/pf $(PREFIX)/bin

uninstall:
	rm -rf $(PREFIX)/share/prim
	rm -rf $(PREFIX)/include/prim
	rm -rf $(PREFIX)/lib/libprim_*.a
	rm -rf $(PREFIX)/bin/sc
	rm -rf $(PREFIX)/bin/pf



base-deps.test:
	@echo "FIXME: add configure time tests"
