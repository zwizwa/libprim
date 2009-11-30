#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"

leaf_object *console_rpc(console *d, const char *cmd) {
    console_write_raw(d, cmd, strlen(cmd));
    console_write_raw(d, "\n", 1);
    port_flush(d->out);
    return console_read(d);
}


int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, argv);
    port *p = port_file_new(stderr, "stderr");


    console *c = _sc_start_console(sc);


    for(;;) {
        leaf_object *o = console_rpc(c, "(+ 1 2)");
        // fprintf(stderr, "o = %p\n", o);
        leaf_write(o, p);
        leaf_free(o);
        sleep(3);
    }
    


    //// STRING IO
    // const char *out = _sc_repl_cstring(sc, "(procedures)");
    // const char *out = _sc_repl_cstring(sc, "foo");
    // fprintf(stderr, "%s\n", out);

    // _sc_eval_cstring(sc, "(write 123)");

    /* The variable `init-script' is defined by _sc_new() based on
       command line arguments. Evaluating it will either start the
       REPL or load a script. */
    _sc_top(sc, CONS(SYMBOL("eval"), CONS(SYMBOL("init-script"), NIL)));
    for(;;) {
        const char *in =  _sc_yield(sc, "foo");
        fprintf(stderr, "got %s\n", in);
    }
    return 0;
}
