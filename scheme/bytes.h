#ifndef _BYTES_H_
#define _BYTES_H_

#include <stdio.h>

typedef struct _bytes bytes;
typedef void (*bytes_free)(bytes *p);

typedef struct {
    bytes_free free;
} bytes_class;

struct _bytes {
    bytes_class *type;
    char *bytes;
    size_t bufsize;
};

bytes* bytes_new(bytes_class *type, size_t size);
bytes_class* bytes_class_new(void);
bytes* bytes_from_cstring(bytes_class *type, const char *str);

void bytes_write_string(bytes *b, FILE *f);

#endif
