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
    size_t size;     // nb of used bytes
    size_t bufsize;  // max bytes in buffer
};

bytes* bytes_new(bytes_class *type, size_t size);
bytes_class* bytes_class_new(void);
bytes* bytes_from_cstring(bytes_class *type, const char *str);

void bytes_write_string(bytes *b, FILE *f);

/* C strings need one extra byte to contain the zero terminator.  This
   is not included in the buffer's `size' field. 

   FIXME: using a raw byte buffer's data as a C string is an ERROR.
   It's necessary to check if it is properly terminated, or access it
   through this function.  */

char *cstring_from_bytes(bytes *b);
void bytes_dump(bytes *b, FILE *f);

#endif
