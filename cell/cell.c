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
