#ifndef _BYTES_H_
#define _BYTES_H_

#include <stdio.h>
#include <leaf/error.h>
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
    size_t bufsize;  // max bytes in buffer (const == 0)
};

bytes *bytes_copy(bytes* b);
bytes* bytes_buffer_new(size_t bufsize);  // same, with size = 0
bytes *bytes_const_new(const char *str, size_t size);

/* FIXME: Deprecated.  This is confusing: it allocates an
   un-initialize buffer.  Use bytes_buffer_new() and bytes_allot()
   instead. */
bytes *bytes_new(size_t size, size_t bufsize);


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

char *cstring_from_bytes(const bytes *b);
int bytes_dump(bytes *b, port *p);     // raw dump
int bytes_hexdump(bytes *b, port *p);  // human-readable


/* Allocate a buffer segment + return pointer. */
char *bytes_allot(bytes *b, size_t size);

int bytes_vprintf(bytes *b, const char *fmt, va_list ap);

#endif
