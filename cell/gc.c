/* Garbage collector small RAM (embedded) target with:
     - CELL store: no arbitrary size vectors
     - Small cell size: 32 bit per cell (1 pair),  
     - Only for small number of CELLs (16 address bits / 256 kB CELL RAM)
     - Pointer reversing MARK phase to eliminate stack.
     - Lazy SWEEP.

     
   GC knows about 2 kinds of CELLs:  PAIR and ATOM.
   
   ATOM is abstract, low 2 bits need to be zero.

   ATOMs are marked during GC to allow for atom-specific GC,
   i.e. refcounting.

   No need for compacting:
      - CELLs are same size so no fragmentation
      - designing for SRAM embedded target: no need for locality

   ( If compaction is necessary the 2-finger algorithm is a good
   candidate [Edwards 1974].  Not local but that's not a problem for
   SRAM.  Otherwise for ordered compaction LISP2 algo? )

   The upper 256 cell addresses are used for representing bytes.  This
   is useful for interpreters (bytecodes + lexical varrefs).


   http://www.slideshare.net/khuonganpt/basic-garbage-collection-techniques

*/

#include <cell/gc.h>
#include <string.h>

#ifdef HEAP_STATIC
cell heap[heap_size];
cell_tag_word _heap_tag[heap_tag_words];
#else
cell *heap;
cell_tag_word *_heap_tag;
int  heap_size;
#endif

static int heap_free;
static cell **roots;

cell_tag_word heap_tag_phase = 0;


void trap(void) { exit(1); }

void mark_cells_prepare(void) {
    /* Before marking from root cells, set all cells to free. */

    /* 1. All cells < heap_free are marked as used.  Physically mark
       _used_ the remaining cells.  This step is necessary when the GC
       is called manually.  */

    if (heap_free < heap_size) {
        int i, tag_free = heap_free / cells_per_tag_word;
        if (heap_tag_phase == 0) {
            for (i=tag_free; i<heap_tag_words; i++) {
                _heap_tag[i] &= ~HEAP_TAG_FREE_MASK;
            }
        }
        else {
            for (i=tag_free; i<heap_tag_words; i++) {
                _heap_tag[i] |= HEAP_TAG_FREE_MASK;
            }
        }
    }

    /* 2. Change the whole heap from used to free. */
    heap_tag_phase ^= HEAP_TAG_FREE_MASK;
}
void heap_clear(void) {
    /* Mark all cells as TAG_PAIR_FREE. */
    // int i;
    heap_tag_phase = 0;
    heap_free = 0;
    memset(_heap_tag, 0xFF, sizeof(_heap_tag));
}


void cell_display_i(word i);

/* For CDR-linked (improper) lists, use (dotted) list notation. */
void cell_display_pair(cell *c) {
    DISP("(");
    cell *cnext;
  next:
    cnext = CDR(c);
    if (cell_is_pair(cnext)) {
        cell_display_i(icar(c));
        DISP(" ");
        c = cnext;
        goto next;
    }
    else {
        cell_display_i(icar(c));
        if (NIL != cnext) {
            DISP(" . ");
            cell_display_i(icdr(c));
        }
    }
    DISP(")");
}
void cell_display_i(word i) {
    if (i >= heap_size) {
        if (i >= HEAP_NUMBER_INDEX) DISP("%d",(int) i & 0xFF);
        else {
            // const char *magic;
            switch (i) {
            case IVOID:  DISP("#<void>"); break;
            case INIL:   DISP("()"); break;
            case IFALSE: DISP("#f"); break;
            case ITRUE:  DISP("#t"); break;
            case IMT:    DISP("#<mt>"); break;
            default:     DISP("#<invalid:%x>", (int)i); break;
            }
        }
    }
    else if (cell_is_pair(heap + i)) {
        cell_display_pair(heap + i);
    }
    else {
        DISP("%p", heap[i].atom);
    }

}
void cell_display(cell *c) { cell_display_i(c - heap); }

void newline(void) {
    DISP("\n");
}

void *mark_atom_inc(void *ptr) {
    DISP("atom_inc: %p\n", ptr);
    return ptr;
}
void *mark_atom_dec(void *ptr) {
    DISP("atom_dec: %p\n", ptr);
    return ptr;
}


/* MARK using pointer reversal. 

   The GC mark phase needs to visit all nodes reachable from the root.
   This can be done using a depth first search which in general needs
   a descent stack to keep track of the return path.

   However if we're allowed to modify the graph in-place, it is
   possible to store the traversal context in the graph nodes, as long
   as we can save extra information about where the pointer is stored.
   For CONS cells this is just one bit: store in CAR or CDR.

   It turns out that we don't need an extra bit, just one extra state
   in the tag space that indicates a "half-visited" node.

   In order to make sense of the bookkeeping we model the graph (tree)
   traversal as a CK machine, where the current code is a cell to be
   interpreted and the current continuation is a previously visited
   pair cell with a back pointer stored in CAR or CDR position as
   indicated by its tag bit.
 
   The machine has 2 operations:
     - invoke code, which descends into the tree, updating the contination
     - invoke (update) continuation



   FREE                CAR                CDR

   ^   ^                  ^              ^
   |   |                  |              |
 [ x | x ]          [ x | x ]          [ x | x ]
                      |                      |
                      v                      v   

 */



void mark_cells(cell *root) {
    cell *c = root;  // subtree under investigation
    cell *k = NIL;   // continuation encoding the rest of traversal

    cell *tmp;
    cell *heap_endx = heap + heap_size;

    goto c_continue;

    /* Invoke the current code. */
  c_continue:

    /* The unused cell address space is ignored by GC, so it can be
       used to encode other (const) values like small ints. */
    if (c >= heap_endx) {
        goto k_return;
    }

    /* Handle current subtree rooted at c. */
    switch(cell_tag(c)) {

    case TAG_PAIR_FREE:
        /* Push continuation, reusing CAR slot of code node. */
        tmp = pcar(c);                   // descend into new code
        c->pair = make_pair(k, pcdr(c)); // create new k frame
        cell_set_tag(c, TAG_K_CAR);
        k = c;     
        c = tmp;
        goto c_continue;

    case TAG_ATOM_FREE:
        cell_set_tag(c, TAG_ATOM);
        goto k_return;

    default:
        /* Nothing to do, invoke continuation. */
        goto k_return;
    }


    /* Invoke the continuation at k when the current subtree is done. */
  k_return:
    if (k == NIL) return;
    switch(cell_tag(k)) {

    case TAG_K_CAR:
        /* Ajust continuation encoding, moving parent frame link from
           CAR to CDR node. */
        tmp = pcdr(k);                   // descend into new code
        k->pair = make_pair(c, pcar(k)); // update k frame in-place
        cell_set_tag(k, TAG_K_CDR);
        c = tmp;
        goto c_continue;
    
    case TAG_K_CDR:
        /* Pop continuation frame. */
        tmp = pcdr(k);                    // pop k frame
        k->pair = make_pair(pcar(k), c);  // restore node
        /* Tag is still TAG_MARKED == TAG_K_CDR */
        c = k;
        k = tmp;
        goto k_return;

    default:
        /* Not reached. */
        trap();
    }

}


int heap_used(void) {
    int used = 0;
    int i;
    for (i = 0; i < heap_size; i++) {
        if (!TAG_IS_FREE(icell_tag(i))) used++;
    }
    return used;
}

static const int gc_stat = 1;
static const int gc_debug = 0;
void heap_collect(void) {
    mark_cells_prepare();
    cell **r;
    for (r = roots; *r; r++) { 
        mark_cells(*r); 
    }
    heap_free = 0; // lazy sweep

    if (gc_stat) {
        DISP("GC: %d / %d\n", heap_used(), (int)heap_size);
    }
    if (gc_debug) {
        DISP("roots: \n");
        for (r = roots; *r; r++) {
            DISP("    ");
            cell_display(*r); 
            newline();
        }
    }
}

/* Finalizers for atoms. */
void atom_finalize(cell *cell) {
    // DISP("finalizing %p (cell: %p)\n", cell->atom, cell);
}

/* Alloc uses lazy free list. */
cell *heap_alloc(int tag) {
    int tries = 2;
    while(tries--) {
        /* Scan for a free cell using tag bitmap word access for
           efficiency. */
        int i = heap_free;
        TAG_INDEX(heap_free, itag, ishift);
        while(itag < heap_tag_words) {
            cell_tag_word w = heap_tag(itag);
            while(ishift < bits_per_tag_word) {
                int t = (w >> ishift) & tag_mask;
                /* Reclaim free pairs or atoms that can be freed. */
                if ((TAG_IS_FREE(t))) {

                    /* If it's an atom we might need to run a
                       finalizer. */
                    if (t == TAG_ATOM_FREE) {
                        atom_finalize(heap+i);
                    }

                    TAG_SET(itag, ishift, tag);
                    heap_free = 1 + i;
                    /* Init to something innocent. */
                    heap[i].pair = make_ipair(INIL, INIL); 
                    return heap + i;
                }
                ishift += tag_bits;
                i++;
            }
            ishift = 0;
            itag++;
        }

        /* None found, collect garbage. */
        heap_collect();
    }
    /* GC doesn't fix it. */
    DISP("GC: heap full.\n");
    trap();
    return NULL;
}


/* Public interface */
cell *heap_cons(cell *a, cell *d) {
    cell *c = heap_alloc(TAG_PAIR);
    c->pair = make_pair(a, d);
    return c;
}
cell *heap_atom(void *ptr) {
    cell *c = heap_alloc(TAG_ATOM);
    c->atom = ptr;
    return c;
}
void heap_set_cons(cell *c, cell *a, cell *d) {
    c->pair = make_pair(a, d);
}
void heap_set_roots(cell **r) { roots = r; }


/* TODO

   - Atoms are currently just (abstractly) marked, not collected.
     This needs to plug into the atom memory manager.  Several are
     possible:

      - All ATOMS are constant pointers (i.e. Program Flash objects)
      - All ATOMS are numbers
      - Use 8-byte aligned atoms and use the extra pointer bit as mark bit.
      - For refcounted atoms, call RC++ on mark, and RC-- for ATOMS in heap
 */
