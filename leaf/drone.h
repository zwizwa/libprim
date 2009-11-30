/* Structured pipe/socket based data communication drone, using the
   leaf/ data types. 

   This can be used to provide a structured data representation on one
   side of an s-expression channel, without having to represent an
   EX-based memory model.

   DRONE -> PIPE   strings
   PIPE -> DRONE   s-expression reader

*/

#ifndef _LEAF_DRONE_H_
#define _LEAF_DRONE_H_

#include <leaf/port.h>
#include <leaf/parser.h>

typedef struct { leaf_class super; } drone_class;

typedef struct {
    drone_class *type;
    parser *p;
    port *in;
    port *out;
} drone;

// FIXME: extend this with a tuple/symbol/bytes based write procedure.
int drone_write_raw(drone *d, void *buf, size_t size);
leaf_object *drone_read(drone *d);
drone *drone_new(port *in, port *out);


#endif

