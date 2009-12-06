#ifndef _BYTES_H_
#define _BYTES_H_

#include <stdio.h>
#include <leaf/leaf.h>
#include <leaf/port.h>

typedef void (*bytes_free)(bytes *p);

typedef struct {
    leaf_class super;
} bytes_class;


struct _bytes {
    leaf_object base;
    char *bytes;
    size_t size;     // nb of used bytes
    size_t bufsize;  // max bytes in buffer
};

bytes *bytes_copy(bytes* b);
bytes* bytes_new(size_t bufsize);
bytes* bytes_buffer_new(size_t bufsize);  // same, with size = 0

// void *bytes_realloc(bytes *b, size_t size);

leaf_class* bytes_type(void);
bytes* bytes_from_cstring(const char *str);
bytes* bytes_from_qcstring(const char *str);

int bytes_write_string(bytes *b, port *p);

/* C strings need one extra byte to contain the zero terminator.  This
   is not included in the buffer's `size' field. 

   FIXME: using a raw byte buffer's data as a C string is an ERROR.
   It's necessary to check if it is properly terminated, or access it
   through this function.  */

char *cstring_from_bytes(bytes *b);
int bytes_dump(bytes *b, port *p);     // raw dump
int bytes_hexdump(bytes *b, port *p);  // human-readable


/* Allocate a buffer segment + return pointer. */
char *bytes_allot(bytes *b, size_t size);


#endif
