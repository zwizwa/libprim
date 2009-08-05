
#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "symbol.h"

struct _symstore {
    atom_class op;
    int nb_syms;
    int total;
    symbol *syms;
};

/* All atoms that appear in GCd vectors have a class pointer as first
   member.  The class struct contains the free() method. */
struct _symbol {
    symstore *s;
    const char* name;
};

int atom_is_symbol(atom *a, symstore *s) {
    return (a->op == &(s->op));
}

symbol string_to_symbol(symstore *s, const char *str){
    int i;
    for (i=0; i<s->nb_syms; i++){
        if (!strcmp(s->syms[i]->name, str)) return s->syms[i];
    }
    // FIXME: grow store
    if (s->nb_syms = s->total) exit(1);

    symbol sym = malloc(sizeof(*s));
    sym->s = s;
    sym->name = malloc(1 + strlen(str));
    strcpy((char *)sym->name, str);
    s->syms[i] = sym;
    s->nb_syms++;
    return sym;
}

const char *symbol_to_string(symstore *s, symbol sym) {
    return sym->name;
}

symstore *symstore_new(int total) {
    symstore *s = malloc(sizeof(*s));
    s->op.free = NULL;
    s->nb_syms = 0;
    s->total = total;
    s->syms = malloc(sizeof(symbol) * total);
    return s;
}
