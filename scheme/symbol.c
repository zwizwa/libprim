
#include <stdlib.h>
#include <string.h>

#include "symbol.h"
#include "gc.h"
#include "gc_config.h"


symbol *string_to_symbol(symstore *s, const char *str){
    int i;
    for (i=0; i<s->nb_syms; i++){
        if (!strcmp(s->syms[i]->name, str)) return s->syms[i];
    }
    // FIXME: grow store
    if (s->nb_syms == s->total) exit(1);

    symbol *sym = malloc(sizeof(*sym));
    sym->s = s;
    sym->name = malloc(1 + strlen(str));
    strcpy((char *)sym->name, str);
    s->syms[i] = sym;
    s->nb_syms++;
    return sym;
}

const char *symbol_to_string(symstore *s, symbol *sym) {
    return sym->name;
}

symstore *symstore_new(int total) {
    symstore *s = malloc(sizeof(*s));
    s->nb_syms = 0;
    s->total = total;
    s->syms = malloc(sizeof(symbol) * total);
    return s;
}
