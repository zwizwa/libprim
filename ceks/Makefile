all:
	gcc -c -g ceks.c
	gcc -c -g gc.c
	gcc -c -g symbol.c
	gcc -c -g test.c
	gcc -o gc-test test.o ceks.o gc.o symbol.o

clean:
	rm -rf gc-test *.o *~
