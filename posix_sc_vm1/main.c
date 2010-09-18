#include <stdio.h>
#include <stdlib.h>
#include <sc_vm1/vm1.h>
#include <ex/ex.h>
#include <leaf_posix/channel.h>
#include <ex_posix/ex_posix.h_prims>

/* Instantiate the posix types here. */
DEF_AREF_TYPE(ck)
DEF_AREF_TYPE(channel)

/* Build an init table for the dictionary. */
static prim_def ex_posix_prims[] = ex_posix_table_init;

int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, (const char**)argv);
    _sc_def_prims(sc, ex_posix_prims);
    _sc_continue(sc);
    return 0;
}
