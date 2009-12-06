#ifndef _PARSER_H_
#define _PARSER_H_

#include <leaf/scanner.h>
#include <leaf/tuple.h>
#include <leaf/symbol.h>

typedef struct _parser parser;
typedef void* (*parser_atom)(void *ctx, const bytes*);
typedef void* (*parser_ob)(void *ctx);
typedef void* (*parser_cons)(void *ctx, void *a1, void *a2);
typedef void* (*parser_vector)(void *ctx, void *lst);

struct _parser {
    leaf_object base; // FIXME: implement leaf object api
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


/* Using the default data constructors the parser converts textual
   s-expressions to symbol-tagged tuples for all constructors, where
   `cons' is the constructor used to construct cdr-linked lists.  This
   is the most general representation that requires only `tuple',
   `symbol' and `bytes' data structures from the leaf/ tree.

   In practice (interfacing with C or other languages) it's simpler to
   use data structures not based on CONS cells, but on flat
   constructors.

   I.e. it's simpler to map the string "(foo 1 2)" to the tuple

     (foo (number "1") (number "2"))

   than a cons-list representation

     (cons (number "1") (cons (number "2") (nil)))


   The routine 'ast_decons' performs this flattening.

*/

tuple *tuple_ctor(tuple *t, symbol *tag, int args);
tuple *tuple_ast_flatten_lin(tuple *in);  //linear: argument is consumed

#endif
