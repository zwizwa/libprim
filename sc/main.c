#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"

int main(int argc, char **argv) {
    sc *sc = _sc_new(NULL, argv[1]);
    _sc_top(sc, CONS(SYMBOL("repl"), NIL));
    return 0;
}
