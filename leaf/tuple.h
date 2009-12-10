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
    leaf_object base;
    int size; // nb of object slots
    leaf_object *slot[0];
};

tuple *tuple_new(int size);
leaf_class *tuple_type(void);

#endif


/* CONS-style stacks/lists using 2-component tuples. */
tuple *tuple_stack_push(tuple *stack, leaf_object *x);
tuple *tuple_stack_drop(tuple *stack);
tuple *tuple_list_remove(tuple *list, leaf_predicate fn, void *ctx);
leaf_object *tuple_list_find(tuple *list, leaf_predicate fn, void *ctx);
leaf_object *tuple_list_find_object(tuple *list, leaf_object *x);
