#ifndef _SCANNER_H_
#define _SCANNER_H_

/* Scheme-style s-expression scanner.  This produces bytes objects
   where the first character represents the token tag. */

#define TOK_STRING      '"'
#define TOK_NUMBER      '0'
#define TOK_HEX_NUMBER  'x'
#define TOK_SYMBOL      ':'
#define TOK_CHAR        '$'
#define TOK_HASH        '#'

#define TOK_QUASI_QUOTE      '`'
#define TOK_QUOTE            '\''
#define TOK_UNQUOTE          ','
#define TOK_UNQUOTE_SPLICING '@'

#define TOK_VLEFT       '<'
#define TOK_LEFT        '('
#define TOK_DOT         '.'
#define TOK_RIGHT       ')'

#define TOK_EOF         'E'
#define TOK_ERROR       '?'

#define TOK_FALSE       'F'
#define TOK_TRUE        'T'


#include <leaf/port.h>
#include <leaf/bytes.h>

typedef struct _scanner scanner;
typedef void (*eof_m)(scanner *x);
struct _scanner {
    port *p;
    bytes *b;

    // these start at 1
    int line;
    int col;
};

typedef bytes token;

const bytes *scanner_token(scanner *x);
void scanner_read(scanner *x);

scanner *scanner_new(port *p);

/* FIXME: this isn't a leaf object.  Also, the _free method doesn't
   recursively free the port. */
void scanner_free(scanner *x);


#endif
