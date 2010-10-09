
#ifndef _CELL_H_
#define _CELL_H_

/* Cell memory for memory-constrained targets (ARM, 64kB).

   Written from scratch, though primed by PICBIT[1] paper.

   [1] http://w3.ift.ulaval.ca/~dadub100/files/picbit.pdf
 */

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
#define heap_size 20
extern cell heap[heap_size];
#else
extern int heap_size;
extern cell *heap;
#endif

void heap_clear(void);

cell *heap_cons(cell *a, cell *d);
cell *heap_atom(void *ptr);

void heap_set_cons(cell *c, cell *a, cell *d);
void heap_set_root(cell *c);

#define CAR_SHIFT 2
#define CDR_SHIFT 17
#define CELL_MASK 0x7FFF

static inline int pair_tag(pair p)     { return p & 3; }
static inline int cell_tag(cell c)     { return pair_tag(c.pair); }
static inline int cell_is_pair(cell c) { return (TAG_ATOM != cell_tag(c)); }

/* Part of the cell address space can be used for small integers.  The
   GC ignores cell pointers that point beyond the heap. */
static inline int   icar(pair c)  { return (c >> CAR_SHIFT) & CELL_MASK; }
static inline int   icdr(pair c)  { return (c >> CDR_SHIFT) & CELL_MASK; }
static inline cell* pcar(pair c)  { return heap + icar(c); }
static inline cell* pcdr(pair c)  { return heap + icdr(c); }

static inline pair cons_tag(int tag, cell *car, cell *cdr) {
    int icar = (car - heap) & CELL_MASK;
    int icdr = (cdr - heap) & CELL_MASK;
    return tag | (icar << CAR_SHIFT) | (icdr << CDR_SHIFT);
}

/* All cells with index >= heap_size are ignored by GC.  This gives
   some room for encoding magic values. 

   - Numbers are in the last 8 bit slot.
   - Special values are in the next-to-last 8 bit slot.
*/

#define HEAP_NUMBER_INDEX (CELL_MASK - 0xFF)
#define HEAP_MAGIC_INDEX  (HEAP_NUMBER_INDEX - 0x100)
static inline cell *heap_number(int n) { 
    return heap + HEAP_NUMBER_INDEX + (n & 0xFF); 
}

#define INIL   (HEAP_NUMBER_INDEX-1)
#define IVOID  (HEAP_NUMBER_INDEX-2)
#define ITRUE  (HEAP_NUMBER_INDEX-3)
#define IFALSE (HEAP_NUMBER_INDEX-4)

#define NIL    (heap + INIL)
#define VOID   (heap + IVOID)
#define TRUE   (heap + ITRUE)
#define FALSE  (heap + IFALSE)




/* 
   Untyped cell access :: cell* -> cell*
*/
#define CAR(c)        pcar(c->pair)
#define CDR(c)        pcdr(c->pair)
#define SET_CAR(c, v) heap_set_cons(c, v, CDR(c))
#define POP(c)        ({cell *x = CAR(c); c = CDR(c); x;})
#define PUSH(c,v)     {c = CONS(v,c);}
#define CONS(a,b)     heap_cons(a,b)

/* Get low 8 bits of cell address. */
#define NCAR(c)       (icar(c->pair) & 0xFF)
#define NCDR(c)       (icdr(c->pair) & 0xFF)
#define NPOP(c)       ({int i = NCAR(c); c = CDR(c); i;})



#endif
