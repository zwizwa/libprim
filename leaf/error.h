/* Primitive exceptions. */

#ifndef _LEAF_ERROR_H_
#define _LEAF_ERROR_H_

#include <leaf/bytes.h>
#include <leaf/symbol.h>

/* Return two arguments: a tag that can be used to dispatch on, and a
   human readable error message.  The latter can be allocated on the C
   stack before calling.  It will be copied before the stack is
   aborted. */
void leaf_error(symbol *error_tag, const char *error_msg);


#endif
