all: scheme

.PHONY: scheme
scheme:
	make -C sc
	make -C pf

clean:
	make -C leaf clean
	make -C object clean
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




