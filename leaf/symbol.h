#ifndef _SYMBOL_H_
#define _SYMBOL_H_

#include <leaf/leaf.h>

typedef struct _symbol  symbol;
typedef struct _symbol_class symbol_class;

/* All atoms that appear in GCd vectors have a class pointer as first
   member.  The class struct contains the free() method. */
struct _symbol {
    symbol_class *type;
    const char* name;
};

struct _symbol_class {
    leaf_class super;
    int nb_syms;
    int total;
    symbol **syms;
};

symbol *symbol_from_cstring(const char *str);
const char *symbol_to_cstring(symbol *sym);
symbol_class *symbol_type();

#endif
