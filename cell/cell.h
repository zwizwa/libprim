
#ifndef _CELL_H_
#define _CELL_H_

// TAG BITS
#define TAG_ATOM  0  /* External data. */
#define TAG_K_CAR 1  /*                     K pointer in CAR. */
#define TAG_K_CDR 2  /* Fully marked cell / K pointer in CDR. */
#define TAG_FREE  3  /* Unmarked cell (free) */

/* The CDR and MARKED tags are shared as they are used for tagging for
   different interpretations of the memory graph.

   - Mark traversal continuations knows about K_CAR / K_CDR continuations.

   - Mark traversal tree descent konw about FREE, ATOM and MARKED (other).

   - Pointer type interpretation knows about ATOM and !ATOM.
*/
#define TAG_MARKED    TAG_K_CDR


typedef long long pair;
typedef void*     atom;

/* The heap is an array of cells.  A cell is either an external
   pointer to an atomic data structure (GC doesn't need to descend),
   or a pair containing two cell pointers and GC mark and continuation
   tags. */
union _cell {
    pair pair;
    atom atom;
};
typedef union _cell cell;

#define HEAP_STATIC

#ifdef HEAP_STATIC
#define heap_size 100
extern cell heap[heap_size];
#else
extern int heap_size;
extern cell *heap;
#endif

/* NIL is an ATOM cell wrapping a NULL pointer; stored in the first
   cell of the heap.  It is never collected. */
#define NIL heap

void heap_clear(void);

cell *heap_cons(cell *a, cell *d);
cell *heap_atom(void *ptr);

void heap_set_cons(cell *c, cell *a, cell *d);
void heap_set_root(cell *c);

static inline int pair_tag(pair p)     { return p & 3; }
static inline int cell_tag(cell c)     { return pair_tag(c.pair); }
static inline int cell_is_pair(cell c) { return (TAG_ATOM != cell_tag(c)); }

/* Part of the cell address space can be used for small integers.  The
   GC ignores cell pointers that point beyond the heap. */
static inline int   icar(pair c)  { return (c >>  2) & 0x7FFF; }
static inline int   icdr(pair c)  { return (c >> 17); }
static inline cell* car(pair c)   { return heap + icar(c); }
static inline cell* cdr(pair c)   { return heap + icdr(c); }

static inline pair cons_tag(int tag, cell *car, cell *cdr) {
    int icar = (car - heap) & 0x7FFF;
    int icdr = (cdr - heap) & 0x7FFF;
    return tag | (icar << 2) | (icdr << 17);
}



#endif
