#ifndef _CTASSERT_H_
#define _CTASSERT_H_

/*  ctassert.h - Macros for evaluation of compile time assertions.

In the following, "this software" refers to ctassert.h - this file,
and this file only.

This software is written in 2011 by Tom Schouten with ownership and
associated rights transferred to Ubidata SA/NV.

To the extent possible under law, Ubidata SA/NV has dedicated all
copyright and related and neighboring rights to this software to the
public domain worldwide.  This software is distributed without any
warranty.  You should have received a copy of the CC0 Public Domain
Dedication along with this software. If not, see:

http://creativecommons.org/publicdomain/zero/1.0/

*/


/*

The CT_ASSERT macro is a cleanroom design from the following specs:

  * Create a macro that can perform assertions at compile time.  I.e.:
    CT_ASSERT(sizeof(int) == 4)

  * Implementation hint: the macro triggers compilation error by
    constructing a negative size data structure.

The GENSYM and CONCAT macros are idioms that are part of C programming
folklore.  I do not know their origin.

[1] http://creativecommons.org/weblog/entry/27081

*/


/* Given a GENSYM macro to handle the generation of unique symbols to
   be used as array names, the CT_ASSERT macro is quite
   straightforward.

   From a boolean input, construct a struct with an array element of
   size 1 when true and size -1 when false.  The latter then triggers
   the compilation error.

   This can be done by composing comparison ==0 and an arithmic
   expression that maps the boolean values 1 and 0 to the integers -1
   and +1 respectively. */

#define CT_ASSERT(x) \
     struct GENSYM(_ctassert_){ char assert[1 - 2*((x)==0)]; }

/* GENSYM: generate a symbol with a unique suffix. */

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


/* EXAMPLES */
#if 0
CT_ASSERT(sizeof(int) == 4);
CT_ASSERT(sizeof(void*) == 4);
#endif




#endif

