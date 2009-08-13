#ifndef _PORT_H_
#define _PORT_H_

#include <stdio.h>
#include <stdarg.h>

typedef struct _port port;
typedef void (*port_free)(port *p);

typedef struct {
    port_free free;
} port_class;

struct _port {
    port_class *type;
    FILE *stream;
};

int port_vprintf(port *p, char *fmt, va_list ap);
int port_printf(port *p, char *fmt, ...);
port_class* port_class_new(void);
port *port_new(port_class *type, FILE *f);



#endif
