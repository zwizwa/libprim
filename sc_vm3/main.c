#include <stdio.h>
#include <stdlib.h>
#include <sc_vm1/vm1.h>


int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, (const char**)argv);

    /* A simple mechanism to detach the VM from the main C thread uses
       a 'console' object, which reads raw string s-expressions, and
       returns parsed tuple/symbol/bytes objects. */

#if 0
    if (0) {
        port *p = port_file_new(stderr, "stderr");
        console *c = NULL;
        _sc_start_console(sc, "/tmp/foo-sc", &c, 1);
        for (;;) {
            // const char *cmd =  "(write (+ 1 2))";
            const char *cmd =  "(+ 1 2)";
            if (1) {
                leaf_object *reply = console_rpc(c, cmd);
                if (1) {
                    reply = (leaf_object*)tuple_ast_flatten_lin((tuple*)reply);
                }
                leaf_write(reply, p);
                port_printf(p, "\n");
                leaf_free((leaf_object*)reply);
            }
            else {
                bytes *b = console_rpc_bytes(c, cmd);
                fprintf(stderr, "RV: %s\n", b->bytes);
                leaf_free((leaf_object*)b);
            }
            sleep(1);
        }
    }
#endif

    _sc_continue(sc);
    return 0;
}
