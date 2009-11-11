
#include <stdlib.h>
#include <string.h>

#include <leaf/symbol.h>
#include <leaf/port.h>

static int symbol_write(symbol *s, port *p) {
    return port_printf(p, "%s", s->name);
}

static symbol_class *type = NULL;
static symbol_class *symbol_class_new(int total) {
    symbol_class *s = calloc(1, sizeof(*s));
    s->super.write = (leaf_write_m)symbol_write;
    s->nb_syms = 0;
    s->total = total;
    s->syms = malloc(sizeof(symbol) * total);
    return s;
}
symbol_class *symbol_type(void) {
    if (!type) type = symbol_class_new(1000);
    return type;
}

symbol *symbol_from_cstring(const char *str){
    int i;
    symbol_class *s = symbol_type();
    for (i=0; i<s->nb_syms; i++){
        if (!strcmp(s->syms[i]->name, str)) return s->syms[i];
    }
    // FIXME: grow store
    if (s->nb_syms == s->total) exit(1);

    symbol *sym = malloc(sizeof(*sym));
    sym->type = s;
    sym->name = malloc(1 + strlen(str));
    strcpy((char *)sym->name, str);
    s->syms[i] = sym;
    s->nb_syms++;
    return sym;
}

const char *symbol_to_cstring(symbol *sym) {
    return sym->name;
}

