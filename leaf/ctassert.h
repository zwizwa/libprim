#ifndef _CTASSERT_H_
#define _CTASSERT_H_

/* 

This code is (c) 2011 Tom Schouten, licenced under the WTFPL[1].
Part of the libprim project: http://zwizwa.be/darcs/libprim

The CT_ASSERT macro is original code, cleanroom design from the
following specs:

  * Create a macro that can perform assertions at compile time.  I.e.:
    CT_ASSERT(sizeof(int) == 4)

  * Implementation hint: the macro triggers compilation error by
    constructing a negative size data structure.

The GENSYM and CONCAT macros are idioms that are part of C programming
folklore.  I do not know their origin.

[1] http://sam.zoy.org/wtfpl/
The WTFPL is "public domain" for countries where there is no "public
domain".

*/


/* Given a GENSYM macro to handle the generation of unique symbols to
   be used as array names, the CT_ASSERT macro is quite
   straightforward.

   From a boolean input, construct an array of size 0 when true and
   size -1 when false.  The latter then triggers the compilation
   error.

   This can be done by composing comparison ==0 and negation. */

#define CT_ASSERT(x)                                \
 static const char GENSYM(_ctassert_)[-((x)==0)];


/* GENSYM: generate a symbol with a unique suffix.

   We use non-standard __COUNTER__ variable supported by the GNU C
   Preprocessor.  ( See the Boost metaprogramming library for a
   standards-compliant solution. ) */

#define GENSYM(x) CONCAT(x,__COUNTER__)

/* CONCAT: macro name concatenation.

   The indirection below is necessary due to prescan:
   http://gcc.gnu.org/onlinedocs/cpp/Argument-Prescan.html#Argument-Prescan

   "If an argument is stringified or concatenated, the prescan does
    not occur. If you want to expand a macro, then stringify or
    concatenate its expansion, you can do that by causing one macro to
    call another macro that does the stringification or concatenation."
*/

#define __CONCAT(x,y) x##y
#define CONCAT(x,y) __CONCAT(x,y)


#endif

