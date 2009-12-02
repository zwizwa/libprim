#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"



int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, argv);

    /* A simple mechanism to detach the VM from the main C thread uses
       a 'console' object, which reads raw string s-expressions, and
       returns parsed tuple/symbol/bytes objects. */
    if (0) {
        port *p = port_file_new(stderr, "stderr");
        console *c = _sc_start_console(sc, "/tmp/foo-sc");
        for (;;) {
            if (1) {
                leaf_object *reply = console_rpc(c, "(write (+ 1 2))");
                if (1) {
                    reply = (leaf_object*)tuple_ast_flatten_lin((tuple*)reply);
                }
                leaf_write(reply, p);
                port_printf(p, "\n");
                leaf_free((leaf_object*)reply);
            }
            else {
                bytes *b = console_rpc_bytes(c, "(write (+ 1 2))");
                fprintf(stderr, "RV: %s\n", b->bytes);
                leaf_free((leaf_object*)b);
            }
            sleep(1);
        }
    }


    /* The variable `init-script' is defined by _sc_new() based on
       command line arguments. Evaluating it will either start the
       REPL or load a script. */
    _sc_top(sc, CONS(SYMBOL("eval"), CONS(SYMBOL("init-script"), NIL)));
    return 0;
}
