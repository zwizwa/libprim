all: scheme

.PHONY: scheme
scheme:
	make -C task
	make -C scheme

clean:
	make -C task clean
	make -C scheme clean
