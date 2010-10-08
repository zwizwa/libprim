/* CONS cell storage with GC

   Another graph memory based on 2-element cells (CONS cells).  2-bit
   tagging is used similarly to the EX GC to distinguish 4 basic
   types:

   - integer
   - cell
   - external pointer
   - finalizer

   For deeply embedded targets we might only need cell and integer.

   The GC can be of mark/sweep kind as there is no need for
   compacting.

   Mark needs mark bits.  For equal-size objects these can easily go
   into a separate bitmap.

   Mark needs a stack (use recursion on CAR as this is most likely the
   shallow branch).  However there is a way to handle stack overflows
   (?).

   The stack can be encoded in the graph using pointer reversal
   (zipper?).  Does this need extra space?

   Sweep can be done lazily.

   ( If compation is necessary the 2-finger algorithm is a good
   candidate [Edwards 1974].  Not local but that's not a problem for
   SRAM. )

   http://www.slideshare.net/khuonganpt/basic-garbage-collection-techniques

*/

/* Only defined for 32 bits.  

   A cell is either an atom or a pair.

   An atom is an arbitrary pointer, word aligned.

   A pair can address 2 cells: [ CDR:15 | CAR:15 | TAGS:2 ].

   This gives 15 bit addresses or 32k cells (128k bytes for cells).

   The CAR, CDR addresses are indicess into the cell array a.k.a. heap.
*/

#include <stdlib.h>

// TAG BITS
#define TAG_ATOM  0  /* External word-aligned pointer. */
#define TAG_CAR   1  /* Reverse pointer in CAR. */
#define TAG_CDR   2  /* Reverse pointer in CDR. */
#define TAG_FREE  3  /* Cell is free (not marked). */ 

/* The CDR and MARKED tags are shared as they are used for tagging for
   different interpretations of the memory graph.

   - Mark traversal continuations knows about CAR/CDR continuations.

   - Mark traversal tree descent konw about FREE and !FREE.

   - Pointer type interpretation knows about ATOM and !ATOM.
*/
#define TAG_MARKED    TAG_CDR
#define TAG_FREE_ATOM TAG_CAR // ???

typedef long long pair;
typedef void*     atom;

/* The heap contains cells.  A cell is either an external pointer to
   an atomic data structure (GC doesn't need to descend), or a pair
   containing two cell pointers and GC bookkeeping bits. */
typedef union {
    pair pair;
    atom atom;
} cell;

#define NB_CELLS 100
static cell heap[NB_CELLS];
static cell *heap_free;
#define NIL heap

static inline int pair_tag(pair p)     { return p & 3; }
static inline int cell_tag(cell c)     { return pair_tag(c.pair); }
static inline int cell_is_pair(cell c) { return (TAG_ATOM != cell_tag(c)); }

/* Don't apply these to non-pairs! */
static inline cell *car(pair c)  { return heap + ((c >> 2)  & (0x7FFF)); }
static inline cell *cdr(pair c)  { return heap + ((c >> 17) & (0x7FFF)); }

static inline pair cons_tag(int tag, cell *car, cell *cdr) {
    int icar = (car - heap) & 0x7FFF;
    int icdr = (cdr - heap) & 0x7FFF;
    return tag | (icar << 2) | (icdr << 17);
}

/* Pointer reversal. 

   It is possible to store the traversal context in the pair graph
   (which is effectively a tree since marked nodes are not traversed).

   In order to make sense of the bookkeeping we model it as a CK
   machine, where the current code is an unmarked cell with unknown
   contents and the current continuation is a previously visited pair
   cell with a back pointer stored in CAR or CDR position.
 
   The machine has 2 operations:
     - invoke code, which descends into the tree, updating the contination
     - invoke (update) continuation

*/


void trap(void) { exit(1); }

/*

   FREE                CAR                CDR

   ^   ^                  ^              ^
   |   |                  |              |
 [ x | x ]          [ x | x ]          [ x | x ]
                      |                      |
                      v                      v   

 */

void mark_free(void) {
    int i;
    for (i=0; i<NB_CELLS; i++) {
        /* Mark non-atom nodes as free. */
        if (TAG_ATOM != pair_tag(heap[i].pair)) {
            heap[i].pair |= TAG_FREE;
        }
    }
}
void heap_clear(void) {
    int i;
    for (i=0; i<NB_CELLS; i++) {
        heap[i].pair = TAG_FREE;
    }
    heap_free = heap;
}

// Arbitrary magic variable that can be encoded as a cell pointer.
#define K_MT NIL

void mark_used(cell *root) {
    cell *c = root;   // subtree under investigation
    cell *k = K_MT;   // continuation

    cell *tmp;

    goto do_code;

    /* Invoke the current code. */
  do_code:
    switch(cell_tag(*c)) {

    case TAG_FREE:
        /* Push continuation, reusing CAR slot of code node. */
        tmp = car(c->pair);                            // descend into new code
        c->pair = cons_tag(TAG_CAR, k, cdr(c->pair));  // create new k frame
        k = c;
        c = tmp;
        goto do_code;

    default:
        /* Nothing to do, invoke continuation. */
        goto do_cont;
    }


    /* Invoke the current continuation. */
  do_cont:
    if (k == K_MT) return;
    switch(cell_tag(*k)) {

    case TAG_CAR:
        /* Ajust continuation encoding, moving parent frame link from
           CAR to CDR node. */
        tmp = cdr(k->pair);                             // descend into new code
        k->pair = cons_tag(TAG_CDR, c, car(k->pair));   // update k frame in-place
        c = tmp;
        goto do_code;
    
    case TAG_CDR:
        /* Pop continuation frame. */
        tmp = cdr(k->pair);                               // pop k frame
        k->pair = cons_tag(TAG_MARKED, car(k->pair), c);  // restore node
        k = tmp;
        goto do_cont;

    default:
        /* Not reached. */
        trap();
    }

}

cell *nil;
cell *root;

void heap_collect(void) {
    mark_free();
    mark_used(root);
    heap_free = heap+1; // skip NIL node
}

/* Alloc uses lazy free list. */
cell *heap_alloc(void) {
  again:
    while(heap_free < (heap + NB_CELLS)) {
        if (TAG_FREE == cell_tag(*heap_free++)) {
            heap_free[-1].pair = TAG_ATOM;
            return heap_free-1;
        }
    }
    /* Collect garbage. */
    heap_collect();
    goto again;
}
cell *heap_cons(cell *a, cell *d) {
    cell *c = heap_alloc();
    c->pair = cons_tag(TAG_MARKED, a, d);
    return c;
}
cell *heap_atom(void *ptr) {
    cell *c = heap_alloc();
    c->atom = ptr;
    return c;
}



int main(void) {
    heap_clear();
    nil  = heap_atom(NULL);
    root = heap_cons(nil, nil);
    for(;;) {
        heap_cons(NULL, NULL);
    }
    return 0;
}
