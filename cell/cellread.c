#include <leaf/parser.h>
#include <leaf/port.h>
#include <leaf/bytes.h>

#include <cell/cellvm.h>

static cell *_cons(vm *vm, cell *a, cell *d) { 
    return CONS(a,d); 
}
static cell *_atom(vm *vm, const bytes *b) {
    return NUMBER(atoi(cstring_from_bytes(b)));
}
static cell *_nil(vm *vm) {
    return NIL;
}
static cell *_eof(vm *vm) {
    return EOFOBJ;
}
static cell *_vector(vm *vm, cell *lst) {
    return lst;
}


cell *vm_read(vm *vm, port *port) {
    parser *p = parser_new(LEAF_DUP(port));
    p->ctx    = vm;
    p->cons   = (parser_cons)_cons;
    p->atom   = (parser_atom)_atom;
    p->nil    = (parser_ob)_nil;
    p->eof    = (parser_ob)_eof;
    p->vector = (parser_vector)_vector;

    cell *data = (cell *)parser_read(p);

    leaf_free((leaf_object*)p);
    return data;
}

static port *port_stdin = NULL;

cell *vm_read_stdin(vm *vm) {
    if (!port_stdin) {
        port_stdin = port_file_new(stdin, "stdin");
    }
    return vm_read(vm, port_stdin);
}
