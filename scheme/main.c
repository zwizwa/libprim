#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"
#include "scheme.h_"
#include <setjmp.h>


#include "test.h_"
void test_scheme(sc *sc) {
    _load(sc); // inline fn from generated test.h_
}

int main(int argc, char **argv) {
    test_scheme(_sc_new());
    return 0;
}

