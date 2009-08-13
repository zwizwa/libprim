#ifndef _SYMBOL_H_
#define _SYMBOL_H_

#include "gc_config.h"

typedef struct _symbol  symbol;
typedef struct _symbol_class symbol_class;

/* All atoms that appear in GCd vectors have a class pointer as first
   member.  The class struct contains the free() method. */
struct _symbol {
    symbol_class *type;
    const char* name;
};

struct _symbol_class {
    int nb_syms;
    int total;
    symbol **syms;
};

symbol *string_to_symbol(symbol_class *s, const char *str);
const char *symbol_to_string(symbol_class *s, symbol *sym);
symbol_class *symbol_class_new(int total);

#endif
