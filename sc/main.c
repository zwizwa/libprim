#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"

int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, argv);
    // _sc_top(sc, CONS(SYMBOL("repl"), NIL)); exit(0);
    
    const char *out = _sc_repl_cstring(sc, "(procedures)");
    fprintf(stderr, "%s\n", out);
    // PURE();
    // _sc_top(sc, CONS(SYMBOL("repl-oneshot"), NIL));
    return 0;
}
