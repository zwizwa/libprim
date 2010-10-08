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

/* Only defined for 32 bits.  A cell is [ CDR:15 | CAR:15 | TAGS:2 ]
   which gives 32k cells (128k bytes for cells).  The CAR,CDR
   addresses are indicess into the cell array. 

   Mask bits:

   00   External object pointer, not a cell.
   01   FREE:   not marked
   10   TODO:   marked, left traversed
   11   MARKED: marked, right traversed

*/

typedef long long u32;
typedef u32 cell;

static inline int cell_tags(cell c) { return c & 3; }
static inline int cell_icar(cell c) { (c >> 2)  & (0x7FFF); }
static inline int cell_icar(cell c) { (c >> 17) & (0x7FFF); }
static inline int is_cell(cell c)   { return (0 != cell_tags(c)); }
static inline cell cell_cons(int icar, int icdr) {
    return 3 | (icar << 2) | (icdr << 17);
}

/* Pointer reversal. 

   Before recursing on .car or .cdr pointer, you copy it to a register
   (current) and flip the pointer to point back to previous current.

   Following back pointers, you need to know whether to re-enter the
   other (CDR) branch, or whether to return to parent.

*/

void mark(cell *root) {
    cell *current = root;
    do {
        /* It's an unmarked cell, so enter into it. */
        SWAP(current, current->car);
        

        if (is_cell(_CAR(current))) {
        }

    } while (current != root);
}
