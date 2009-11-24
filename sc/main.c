#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"

int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, argv);

    //// STRING IO
    // const char *out = _sc_repl_cstring(sc, "(procedures)");
    // const char *out = _sc_repl_cstring(sc, "foo");
    // fprintf(stderr, "%s\n", out);

    // _sc_eval_cstring(sc, "(write 123)");

    /* The variable `init-script' is defined by _sc_new() based on
       command line arguments. Evaluating it will either start the
       REPL or load a script. */
    _sc_top(sc, CONS(SYMBOL("eval"), CONS(SYMBOL("init-script"), NIL)));
    return 0;
}
