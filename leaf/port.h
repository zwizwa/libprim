#ifndef _PORT_H_
#define _PORT_H_

#include <stdio.h>
#include <stdarg.h>
#include <leaf/leaf.h>
#include <leaf/error.h>

/* Ports are abstract.  Next to libc FILE ports they can represent
   anything that produces or consumes bytes. */
typedef void (*port_free_m)(port *p);
typedef int (*port_vprintf_m)(port *p, const char *fmt, va_list ap);
typedef int (*port_getc_m)(port *p);
typedef int (*port_ungetc_m)(port *p, int c);
typedef int (*port_putc_m)(port *p, int c);
typedef int (*port_write_m)(port *p, void *buf, size_t len);
typedef int (*port_read_m)(port *p, void *buf, size_t len);
typedef void (*port_close_m)(port *x);
typedef bytes* (*port_bytes_m)(port *x);
typedef void (*port_flush_m)(port *x);

typedef struct {
    leaf_class super; // standard methods
} port_class;

typedef struct {
    /* Operations come from a delegate object. */
    port_vprintf_m vprintf;
    port_getc_m   get;
    port_ungetc_m unget;
    port_putc_m  put;
    port_write_m write;
    port_read_m  read;
    port_close_m close;
    port_bytes_m bytes;
    port_flush_m flush;
} port_methods;

struct _port {
    leaf_object base;
    port_methods *m;
    char *name;
    union {
        struct {
            FILE* file;
            int fd;  // for select()
        } f;
        struct {
            bytes *bytes;
            int read_index;
        } b;
    } stream;
};

int port_vprintf(port *p, const char *fmt, va_list ap);
int port_getc(port *p);
int port_ungetc(port *p, int c);
int port_putc(port *p, int c);
int port_write(port *p, void *buf, size_t len);
int port_read(port *p, void *buf, size_t len);
void port_close(port *x);
void port_flush(port *x);

void port_free(port *x);
int port_printf(port *p, const char *fmt, ...);
leaf_class *port_type(void);
port *port_file_new(FILE *f, const char *name);
port *port_bytes_new(bytes *b);
bytes *port_get_bytes(port *b); // for string ports


#define PORT_SOCKET_SERVER    (1<<0)  // 0 = CLIENT
#define PORT_SOCKET_UDP       (1<<1)  // 0 = TCP
#define PORT_SOCKET_UNIX      (1<<2)  // 0 = INET
#define PORT_SOCKET_BROADCAST (1<<3)

/* Functions that take a leaf_ctx parameter can throw exceptions. */

int fd_socket(leaf_ctx *ctx,
              const char *sockname,  // hostname | filesystem node
              int port_number,       // only for TCP sockets
              int kind);

int fd_accept(leaf_ctx *ctx, int fd);

int fd_pipe(char **argv, int *pid, int connect_fd);
int fd_pipe_2(char **argv, int *pid, int *fd_to_stdin, int *fd_from_stdout);


bytes *port_slurp(port *p);
int port_fd(port *p);

bytes *leaf_to_string(leaf_object *o);


#endif
