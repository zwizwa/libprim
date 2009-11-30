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
            leaf_object *reply = console_rpc(c, "(write (+ 1 2))");
            if (1) {
                leaf_write(reply, p);
                port_printf(p, "\n");
            }
            sleep(3);
        }
    }


    /* The variable `init-script' is defined by _sc_new() based on
       command line arguments. Evaluating it will either start the
       REPL or load a script. */
    _sc_top(sc, CONS(SYMBOL("eval"), CONS(SYMBOL("init-script"), NIL)));
    return 0;
}
