all:
	gcc -c cons.c
	gcc -c gc.c


test:
	gcc -DTEST -g gc.c -o gc-test

clean:
	rm -rf gc-test *.o *~
