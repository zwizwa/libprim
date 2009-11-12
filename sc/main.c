#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"

int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, argv);

    //// STRING IO
    // const char *out = _sc_repl_cstring(sc, "(procedures)");
    // fprintf(stderr, "%s\n", out);

    _sc_top(sc, CONS(SYMBOL("repl"), NIL));
    return 0;
}
