#include <stdlib.h>
#include <stdio.h>

/* Parser for simplified tokenized ADT tree encoding.  All
   constructors are parametric.  The primitive data type is a byte
   string, represented by the tag 'B'.
*/

typedef struct _parser parser;
typedef void (*parse_error)(parser *, const char *);
struct _parser {
    FILE *stream;
    parse_error error;
};

// #define ERROR(msg) {p->error(p, msg); exit(1);}
#define ERROR(...) {fprintf(stderr, __VA_ARGS__); exit(1);}

int number(parser *p) {
    int c, tag, nibble, size;
    size = 0;
    if (EOF == (c = fgetc(p->stream))) return -1; // proper EOF
  next_nibble:
    nibble = c & 0x0F;
    tag    = c & 0xF0;
    size   = (size << 4) + nibble;
    switch(tag) {
    case 0x30: 
        return size;
    case 0x20:
        if (EOF == (c = fgetc(p->stream)))
            ERROR("EOF during number read");
        goto next_nibble;
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
    while((n = number(p)) >= 0) {
        char *b = bytes(p, n);
        printf("%s\n", b);
        free(b);
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
