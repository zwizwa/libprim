all: 
	make -C leaf
	make -C media
	make -C ex
	make -C sc
	make -C pf

clean:
	rm -f *~
	make -C leaf clean
	make -C ex clean
	make -C sc clean
	make -C pf clean

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
	install -d $(PREFIX)/include/prim/ex
	install -d $(PREFIX)/include/prim/leaf
	install -d $(PREFIX)/include/prim/sc
	install -d $(PREFIX)/include/prim/media

	install -m 644 ex/*.h* $(PREFIX)/include/prim/ex/
	install -m 644 sc/*.h* $(PREFIX)/include/prim/sc/
	install -m 644 leaf/*.h* $(PREFIX)/include/prim/leaf/
	install -m 644 media/*.h* $(PREFIX)/include/prim/media/

	install -m 755 */libprim_*.a $(PREFIX)/lib/
