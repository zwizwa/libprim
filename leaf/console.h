/* Structured pipe/socket based data communication console, using the
   leaf/ data types.

   Implements a bi-directional (RPC) link over an s-expression
   channel.  

   The reader end performs a parser step to tuple,symbol,bytes proper
   tree representation.  This is straightforward to interface to any
   external structured data representation.

   The writer end is (currently) raw: it's usually simpler to
   implement s-expression serialization natively.

*/

#ifndef _LEAF_CONSOLE_H_
#define _LEAF_CONSOLE_H_

#include <leaf/port.h>
#include <leaf/parser.h>

typedef struct { leaf_class super; } console_class;

typedef struct {
    console_class *type;
    parser *p;
    port *in;
    port *out;
} console;



leaf_object *console_read(console *d);
console *console_new(port *in, port *out);

// FIXME: extend this with a tuple/symbol/bytes based write procedure.

leaf_object *console_rpc(console *d, const char *cmd);
bytes *console_rpc_bytes(console *d, const char *cmd);

#endif

