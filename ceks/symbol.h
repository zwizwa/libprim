#ifndef _SYMBOL_H_
#define _SYMBOL_H_

#include "gc.h"

typedef struct _symbol  symbol;
typedef struct _symstore symstore;

/* All atoms that appear in GCd vectors have a class pointer as first
   member.  The class struct contains the free() method. */
struct _symbol {
    symstore *s;
    const char* name;
};

struct _symstore {
    atom_class op;
    int nb_syms;
    int total;
    symbol **syms;
};

symbol *string_to_symbol(symstore *s, const char *str);
const char *symbol_to_string(symstore *s, symbol *sym);
symstore *symstore_new(int total);
int atom_is_symbol(atom *a, symstore *s);

#endif
