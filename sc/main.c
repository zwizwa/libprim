#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"
#include "scheme.h_"
#include <setjmp.h>


#include "test.h_"
void dummy(void *x, void *y){}
void test_scheme(sc *sc) {
    dummy(gc_new, gc_mark);  // shut up warnings
    _load(sc); // inline fn from generated test.h_
}

int main(int argc, char **argv) {
    test_scheme(_sc_new());
    return 0;
}
