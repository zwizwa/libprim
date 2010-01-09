#include <stdlib.h>
#include <stdio.h>

/* Parser for serialized ADT tree encoding.  The primitive data type
   is a byte string.  All other data types are expressed using named
   constructors.  There is no restriction on size and contents.
*/

typedef struct _parser parser;
typedef void (*parse_error)(parser *, const char *);
struct _parser {
    FILE *stream;
    parse_error error;
};

// #define ERROR(msg) {p->error(p, msg); exit(1);}
#define ERROR(...) {fprintf(stderr, __VA_ARGS__); exit(1);}

/* Return size of next token. 
   size >= 0   raw byte string (empty tokens allowed)
   size <  0   command token (min size = 1)
*/
   
int marker(parser *p) {
    int c, tag, nibble, size;
    size = 0;
  next_nibble:
    if (EOF == (c = fgetc(p->stream))) {
        /* The idea of this format is to use prefix encoding to make
           the recognizer simpler.  Therefore EOF is always an
           error. */
        ERROR("EOF");
    }
    nibble = c & 0x0F;
    tag    = c & 0xF0;
    size   = (size << 4) + nibble;
    switch(tag) {
    case 0x40: return size;
    case 0x30: return -size;  // token
    case 0x20: goto next_nibble;
    default:   
        ERROR("invalid number tag");
    }
}

char *bytes(parser *p, int size) {
    char *buf = malloc(size + 1);
    if (size != fread(buf, 1, size, p->stream)) 
        ERROR("read error");
    buf[size] = 0;
    return buf;
}

void parse(parser *p) {
    int n;
    char *b;
    for(;;) {
        n = marker(p);
        b = NULL;

        if (n < 0) {
            b = bytes(p, -n); 
            printf("%s\n", b);
        }
        else if (n > 0) {
            b = bytes(p, n); 
            printf("\"%s\"\n", b);
        }
        if (b) free(b); 
    }
}

void print_parse_error(parser *p, const char *msg) {
    fprintf(stderr, "ERROR: %s\n", msg);
    exit(1);
}
int main(void) {
    parser p = {stdin, print_parse_error};
    parse(&p);
    return 0;
}



/* Printer */

//void 
