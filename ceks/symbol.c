
#include <stdlib.h>
#include <string.h>
typedef const char* symbol;

typedef struct {
    int nb_syms;
    symbol *syms;
} symstore;

symbol string_to_symbol(symstore *s, const char *str){
    int i;
    for (i=0; i<s->nb_syms; i++){
        if (!strcmp(s->syms[i], str)) return s->syms[i];
    }
    char *sym = malloc(1 + strlen(str));
    strcpy(sym, str);
    s->syms[i] = sym;
    s->nb_syms++;
    return (symbol)sym;
}

const char *symbol_to_string(symstore *s, symbol sym) {
    return (const char*)sym;
}
