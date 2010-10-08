/* Garbage collector for 32 bit small embedded target with:
     - CELL store: no arbitrary size vectors
     - Small cell size: 32 bit per cell (1 pair),  
     - Only for small number of CELLs (15 address bits / 128 kB CELL RAM)
     - Pointer reversing MARK phase to eliminate stack.
     - Lazy SWEEP.

     
   GC knows about 2 kinds of CELLs:  PAIR and ATOM.
   
   ATOM is abstract, low 2 bits need to be zero.

   ATOMs are marked during GC to allow for atom-specific GC,
   i.e. refcounting.

   CELLs are same size, and we're running on SRAM embedded target, so
   no need for compacting.

   ( If compaction is necessary the 2-finger algorithm is a good
   candidate [Edwards 1974].  Not local but that's not a problem for
   SRAM.  Otherwise for ordered compaction LISP2 algo? )

   The upper 256 cell addresses are used for representing bytes.  This
   is useful for interpreters (bytecodes + lexical varrefs).


   http://www.slideshare.net/khuonganpt/basic-garbage-collection-techniques

*/

#include <stdlib.h>
#include <stdio.h>

#include "cell.h"

#ifdef HEAP_STATIC
cell heap[heap_size];
#else
cell *heap;
int  heap_size;
#endif

static cell *heap_free;
static cell *root;


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
    for (i=0; i<heap_size; i++) {
        /* Mark non-atom nodes as free. */
        if (TAG_ATOM != pair_tag(heap[i].pair)) {
            heap[i].pair |= TAG_FREE;
        }
    }
}
void heap_clear(void) {
    int i;
    for (i=0; i<heap_size; i++) {
        heap[i].pair = TAG_FREE;
    }
    heap_free = heap + 1;
    heap[0].atom = NULL;  // set NIL
}

#define DISP(...) fprintf(stderr, __VA_ARGS__)

void cell_display(cell c);
void cell_display_i(int i) {
    if (i >= 0x7F00) DISP("%d", i & 0xFF);
    else cell_display(heap[i]);
}
void cell_display(cell c) {
    int i;
    if (cell_is_pair(c)) {
        DISP("(");
        cell_display_i(icar(c.pair));
        DISP(" . ");
        cell_display_i(icdr(c.pair));
        DISP(")");
    }
    else {
        if (!c.atom) DISP("()");
        else DISP("%p", c.atom);
    }
}
void newline(void) {
    DISP("\n");
}

void *mark_atom(void *ptr) {
    DISP("atom: %p\n", ptr);
    return ptr;
}

void mark_used(cell *root) {
    cell *c = root;  // subtree under investigation
    cell *k = NIL;   // continuation

    cell *tmp;
    cell *heap_endx = heap + heap_size;

    goto do_code;

    /* Invoke the current code. */
  do_code:

    /* The unused cell address space is ignored by GC, so it can be
       used to encode other (const) values like small ints. */
    if (c >= heap_endx) {
        goto do_cont;
    }

    /* Interpret current cell type */
    switch(cell_tag(*c)) {

    case TAG_FREE:
        /* Push continuation, reusing CAR slot of code node. */
        tmp = car(c->pair);                             // descend into new code
        c->pair = cons_tag(TAG_K_CAR, k, cdr(c->pair)); // create new k frame
        k = c;     
        c = tmp;
        goto do_code;

    case TAG_ATOM:
        /* Atom mark bits are not stored in the cell.  However, we do
           call a hook here to be able to run finalizers if
           necessary. */
        if (c != NIL) { c->atom = mark_atom(c->atom); }
        goto do_cont;

    default:
        /* Nothing to do, invoke continuation. */
        goto do_cont;
    }


    /* Invoke the current continuation. */
  do_cont:
    if (k == NIL) return;
    switch(cell_tag(*k)) {

    case TAG_K_CAR:
        /* Ajust continuation encoding, moving parent frame link from
           CAR to CDR node. */
        tmp = cdr(k->pair);                             // descend into new code
        k->pair = cons_tag(TAG_K_CDR, c, car(k->pair)); // update k frame in-place
        c = tmp;
        goto do_code;
    
    case TAG_K_CDR:
        /* Pop continuation frame. */
        tmp = cdr(k->pair);                               // pop k frame
        k->pair = cons_tag(TAG_MARKED, car(k->pair), c);  // restore node
        c = k;
        k = tmp;
        goto do_cont;

    default:
        /* Not reached. */
        trap();
    }

}



void heap_collect(void) {
    mark_free();
    mark_used(root);
    heap_free = heap+1; // skip NIL node
}

int heap_used(void) {
    int used = 0;
    int i;
    for (i = 0; i < heap_size; i++) {
        if (TAG_FREE != cell_tag(heap[i])) used++;
    }
    return used;
}


/* Alloc uses lazy free list. */
cell *heap_alloc(void) {
  again:
    while(heap_free < (heap + heap_size)) {
        cell *c = heap_free++;
        if (TAG_FREE == cell_tag(*c)) {
            c->pair = TAG_ATOM;
            return c;
        }
    }
    /* Collect garbage. */
    heap_collect();
    DISP("gc: %d cells used\n", heap_used());
    goto again;
}


/* Public interface */
cell *heap_cons(cell *a, cell *d) {
    cell *c = heap_alloc();
    c->pair = cons_tag(TAG_MARKED, a, d);
    return c;
}
cell *heap_atom(void *ptr) {
    cell *c = heap_alloc();
    c->pair = ((cell)ptr).pair & ~3;
    return c;
}
cell *heap_number(int n) {
    /* This is not a valid cell pointer!  Will be ignored by GC and
       can be interpreted by user as number. */
    return heap+0x7F00+n;
}
void heap_set_cons(cell *c, cell *a, cell *d) {
    c->pair = cons_tag(TAG_MARKED, a, d);
}
void heap_set_root(cell *c) { root = c; }


// test
#if 1
int main(void) {
    heap_clear();
    cell *x = NIL;

    // cell *atom = heap_atom((void*)0xDEADBEEF);
    // cell *circ = heap_cons(NIL, NIL);
    // circ->pair = cons_tag(TAG_MARKED, atom, circ);
    // x = circ;

    x = heap_cons(heap_number(3), x);
    x = heap_cons(heap_number(2), x);
    x = heap_cons(heap_number(1), x);
    root = x;
    
    for(;;) {
        cell_display(*root); newline();
        heap_cons(NIL, NIL);
        // printf("used: %d\n", heap_used());
    }
    return 0;
}
#endif


/* TODO

   - Atoms are currently just (abstractly) marked, not collected.
     This needs to plug into the atom memory manager.  Several are
     possible:

      - All ATOMS are constant pointers (i.e. Program Flash objects)
      - All ATOMS are numbers
      - Use 8-byte aligned atoms and use the extra pointer bit as mark bit.
      - For refcounted atoms, call RC++ on mark, and RC-- for ATOMS in heap
      
 */
