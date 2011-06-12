/* Simple s-expression reader on top of leaf/parser.h */

#include <leaf/parser.h>
#include <leaf/port.h>
#include <leaf/bytes.h>

#include <cell/vm.h>

#include <string.h>
#include <stdlib.h>

static cell *_cons(vm *vm, cell *a, cell *d) { 
    return CONS(a,d); 
}
static cell *_atom(vm *vm, const bytes *b) {
    char *s = cstring_from_bytes(b);
    char tok = *s++;
    
    switch(tok) {
    case TOK_TRUE:   return TRUE;
    case TOK_FALSE:  return FALSE;
    case TOK_NUMBER: return NUMBER(atoi(s));
    default:         return VOID;
    }
}
static cell *_nil(vm *vm) {
    return NIL;
}
static cell *_eof(vm *vm) {
    return EOF_OBJECT;
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

