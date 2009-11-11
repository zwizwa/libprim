#ifndef _PARSER_H_
#define _PARSER_H_

#include <leaf/scanner.h>

typedef struct _parser parser;
typedef void* (*parser_atom)(void *ctx, const bytes*);
typedef void* (*parser_ob)(void *ctx);
typedef void* (*parser_cons)(void *ctx, void *a1, void *a2);
typedef void* (*parser_vector)(void *ctx, void *lst);

struct _parser {
    scanner* s;
    void *ctx;
    parser_atom   atom;
    parser_cons   cons;
    parser_vector vector;
    parser_ob     nil;
    parser_ob     eof;
};

/* By default the parser produces symbol tagged tree instances.
   Override atom, cons and nil. */
parser *parser_new(port *prt);
void *parser_read(parser *p);

/* FIXME: this isn't a leaf object.  Also this _free doesn't free the
   port, but it does free the scanner. */
void parser_free(parser *p);

#endif
