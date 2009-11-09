
DIRS = leaf media ex sc pf

all: 
	for dir in $(DIRS); do make -C $$dir; done

clean:
	rm -f *~
	for dir in $(DIRS); do make -C $$dir clean; done

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


PREFIX=~/sw

install: all
	install -d $(PREFIX)/lib/pkgconfig
	install -m 644 libprim.pc $(PREFIX)/lib/pkgconfig

	install -d $(PREFIX)/share/prim/
	install -m 644 sc/*.scm $(PREFIX)/share/prim/

	install -d $(PREFIX)/include/prim/ex
	install -d $(PREFIX)/include/prim/leaf
	install -d $(PREFIX)/include/prim/sc
	install -d $(PREFIX)/include/prim/media

	install -m 644 ex/*.h* $(PREFIX)/include/prim/ex/
	install -m 644 sc/*.h* $(PREFIX)/include/prim/sc/
	install -m 644 leaf/*.h* $(PREFIX)/include/prim/leaf/
	install -m 644 media/*.h* $(PREFIX)/include/prim/media/

	install -m 755 */libprim_*.a $(PREFIX)/lib/

uninstall:
	rm -rf $(PREFIX)/share/prim
	rm -rf $(PREFIX)/include/prim
	rm -rf $(PREFIX)/lib/libprim_*.a


base-deps.test:
	@echo "FIXME: add configure time tests"
