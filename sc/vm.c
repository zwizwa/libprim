/* Attempt to speed up the interpreter using a simple VM.  The idea is:

   - only make continuations and other intermediate structures
     explicit when they are captured

   - link global variables directly to a box
   - use relative offsets for lexical variables

*/


/* Lexical depth is limited to 256.  This seems reasonable, and allows
   the encoding of a variable address in a single integer atom. */
