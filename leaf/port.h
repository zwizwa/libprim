#ifndef _PORT_H_
#define _PORT_H_

#include <stdio.h>
#include <stdarg.h>

typedef struct _port port;
typedef void (*port_free_m)(port *p);

typedef struct {
    port_free_m free;
} port_class;

struct _port {
    port_class *type;
    FILE *stream;
    char *name;
};

int port_vprintf(port *p, const char *fmt, va_list ap);
int port_printf(port *p, const char *fmt, ...);
port_class* port_class_new(void);
port *port_new(port_class *type, FILE *f, const char *name);
int port_getc(port *p);
void port_free(port *x);
void port_close(port *x);

#endif
