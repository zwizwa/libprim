/* ADTs without sharing, used to represent proper trees of leaf
   objects.

   This is used in the parser(s).

   Note that this is different from the 'vector' type in EX/SC/PF,
   which can represent arbitrary graph structure and thus requires
   asynchronous GC.  The destructors in the 'tuple' struct allow only
   proper tree structures.
*/

#ifndef _TUPLE_H_
#define _TUPLE_H_

#include <leaf/leaf.h>

typedef struct _tuple tuple;

typedef struct {
    leaf_class super;
} tuple_class;

struct _tuple {
    tuple_class *type;
    int size; // nb of object slots
    leaf_object *slot[0];
};

tuple *tuple_new(int size);


#endif
