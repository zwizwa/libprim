
#ifndef _CELL_H_
#define _CELL_H_

/* Cell memory for memory-constrained targets (ARM, 64kB).

   Written from scratch, though primed by PICBIT[1] paper.

   [1] http://w3.ift.ulaval.ca/~dadub100/files/picbit.pdf
 */

/* Cell tag bits.  Don't change these as the patterns are used for bit
   twiddling tricks in the GC. */

#define TAG_FREE  3  /* Unmarked cell (free) */
#define TAG_K_CAR 2  /*                     K pointer in CAR. */
#define TAG_K_CDR 1  /* Fully marked cell / K pointer in CDR. */
#define TAG_ATOM  0  /* External data. */


/* Tag sharing is possible due to different interpretations: 

   - Mark traversal continuations knows about K_CAR / K_CDR continuations.

   - Mark traversal tree descent konw about FREE, ATOM and MARKED (other).

   - Pointer type interpretation knows about ATOM and !ATOM.
*/

/* The CDR and MARKED tags are shared which saves tags and eliminates
   one tag update in the mark phase. */
#define TAG_MARKED TAG_K_CDR
#define TAG_PAIR   TAG_MARKED

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

/* Tag bits are stored in a separate table. */
typedef unsigned int cell_tag_word;
#define tag_bits 2
#define tag_mask ((1 << tag_bits) - 1)
#define bits_per_tag_word (8 * sizeof(cell_tag_word))
#define cells_per_tag_word (bits_per_tag_word / tag_bits)
#define heap_tag_words (((min_heap_size - 1) / cells_per_tag_word) + 1)

#ifdef HEAP_STATIC
#define min_heap_size 20  // will be rounded up
#define heap_size (heap_tag_words * cells_per_tag_word)
extern cell heap[heap_size];
extern cell_tag_word heap_tag[heap_tag_words];
#else
extern int heap_size;
extern cell *heap;
extern heap_tag_word *heap_tag;
#endif

void heap_clear(void);
void heap_collect(void);

cell *heap_cons(cell *a, cell *d);
cell *heap_atom(void *ptr);

void heap_set_cons(cell *c, cell *a, cell *d);
void heap_set_roots(cell **r);

#define CAR_SHIFT 0
#define CDR_SHIFT 16
#define CELL_MASK 0xFFFF

#define TAG_INDEX(i, itag, ishift)                         \
    int itag   = i / cells_per_tag_word;                   \
    int ishift = (i % cells_per_tag_word) * tag_bits;

#define CELL_INDEX(itag, ishift) \
    ((itag * cells_per_tag_word) + (ishift / tag_bits))

static inline int icell_tag(int i) {
    TAG_INDEX(i, itag, ishift);
    int tag = (heap_tag[itag] >> ishift) & tag_mask;
    return tag;
}
#define TAG_SET(itag, ishift, tag) {            \
    cell_tag_word w = heap_tag[itag];           \
    w &= ~(tag_mask << ishift);                 \
    w |= (tag & tag_mask) << ishift;            \
    heap_tag[itag] = w;                         \
}

static inline void icell_set_tag(int i, int tag) {
    TAG_INDEX(i, itag, ishift);
    TAG_SET(itag, ishift, tag);
}

static inline int cell_tag(cell *c) { 
    return icell_tag(c - heap); 
}
static inline void cell_set_tag(cell *c, int tag) {
    icell_set_tag(c - heap, tag);
} 

static inline int cell_is_pair(cell *c) { 
    if (c > (heap + heap_size)) return 0;
    return (TAG_ATOM != cell_tag(c)); 
}

/* Part of the cell address space can be used for small integers.  The
   GC ignores cell pointers that point beyond the heap. */
static inline int   icar(cell *c)  { return (c->pair >> CAR_SHIFT) & CELL_MASK; }
static inline int   icdr(cell *c)  { return (c->pair >> CDR_SHIFT) & CELL_MASK; }
static inline cell* pcar(cell *c)  { return heap + icar(c); }
static inline cell* pcdr(cell *c)  { return heap + icdr(c); }

static inline pair  make_ipair(int car, int cdr) {
    int icar = car & CELL_MASK;
    int icdr = cdr & CELL_MASK;
    return (icar << CAR_SHIFT) | (icdr << CDR_SHIFT);
}

static inline pair  make_pair(cell *car, cell *cdr) {
    return make_ipair(car - heap, cdr - heap);
}

/* All cells with index >= heap_size are ignored by GC.  This gives
   some room for encoding numbers and other special values. 

   - Numbers are at the end of the address space
   - Special values are allocated right before number space.
*/

#define HEAP_NUMBER_MASK  0xFF
#define HEAP_NUMBER_INDEX (CELL_MASK - HEAP_NUMBER_MASK)
static inline cell *heap_number(int n) { 
    return heap + HEAP_NUMBER_INDEX + (n & HEAP_NUMBER_MASK);
}
#define NUMBER(x) heap_number(x)
#define ATOM(x) heap_atom(x)

#define INIL   (HEAP_NUMBER_INDEX - 1)
#define IVOID  (HEAP_NUMBER_INDEX - 2)
#define ITRUE  (HEAP_NUMBER_INDEX - 3)
#define IFALSE (HEAP_NUMBER_INDEX - 4)
#define IMT    (HEAP_NUMBER_INDEX - 5)

#define NIL    (heap + INIL)
#define VOID   (heap + IVOID)
#define TRUE   (heap + ITRUE)
#define FALSE  (heap + IFALSE)
#define MT     (heap + IMT)




/* 
   Untyped cell access :: cell* -> cell*
*/
#define CAR(c)        pcar(c)
#define CDR(c)        pcdr(c)
#define SET_CAR(c, v) heap_set_cons(c, v, CDR(c))
#define POP(c)        ({cell *x = CAR(c); c = CDR(c); x;})
#define PUSH(c,v)     {c = CONS(v,c);}
#define CONS(a,b)     heap_cons(a,b)

/* Get low 8 bits of cell address. */
#define NCAR(c)       (icar(c) & 0xFF)
#define NCDR(c)       (icdr(c) & 0xFF)
#define NPOP(c)       ({int i = NCAR(c); c = CDR(c); i;})

#include <stdlib.h>
#include <stdio.h>


/* Reserve a part of CELL address space for constant references to
   objects stored in Flash memory. */



#define DISP(...) fprintf(stderr, __VA_ARGS__)
void cell_display(cell *c);
void newline(void);


#endif
