/* ADTs without sharing, used to represent proper trees of leaf
   objects.

   This is used in the parser(s).

   Note that this is different from the 'vector' type in EX/SC/PF,
   which can represent arbitrary graph structure and thus requires
   asynchronous GC.  The destructors in the 'tree' struct allow only
   proper tree structures.
*/

#ifndef _TREE_H_
#define _TREE_H_

#include <leaf/leaf.h>

typedef struct _tree tree;

typedef struct {
    leaf_class super;
} tree_class;

struct _tree {
    tree_class *type;
    int size; // nb of object slots
    leaf_object *slot[0];
};

tree *tree_new(int size);


#endif
