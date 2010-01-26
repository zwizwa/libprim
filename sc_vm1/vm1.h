#ifndef _SCHEME_H_
#define _SCHEME_H_

#include <setjmp.h>
#include <leaf/leaf.h>
#include <ex/ex.h>
#include <ex/ex.h_prims>
#include <sc/sc.h>
#include <sc/sc.h_prims>

#include <leaf/console.h>



/* The interpreter is written as a step() function manipulating a
   state data structure.  It is based on the CEKS machine from
   http://www.cs.utah.edu/plt/publications/pllc.pdf

   All data structures and function primitives are Scheme values.

   The GC is a stop-and-copy type Cheney algorithm supporting 4 data
   types: integers, vectors, finalizers and constants.  Note that it
   is not safe to perform allocation outside of the main interpreter
   loop, and the main loop is not re-entrant.

   When GC is triggered in the context of a primitive, it will be
   aborted and restarted.  Therefore primitives may not perform vector
   allocation _after_ performing side effects (i.e. mutation of
   globally accessible state, be it interpreter state or foreign
   data).

   Such an interpreter can support two kinds of primitives:

     * RESTARTABLE: Pure functions operating on Scheme data structures
       (or impure functions that do not perform allocation _after_
       mutation).

     * ABSTRACT: C code that does not refer to any Scheme data, and
       thus cannot trigger GC.

   The disadvantage of not being able to access Scheme data from
   impure C code can be largely removed by providing a suspension
   mechanism for primitives.  In this case the C code could behave as
   a coroutine, which allows the use of enumerators / iterators
   instead of construction of intermediate (Scheme) data structures.

*/






/* The machine will attempt to reduce the current term (which is a
   closure = open term + variable bindings) or continue with the
   computation context (a list of reducible closures). */
typedef struct {
    vector v;
    _ redex_or_value;  // naked value or reducible/value closure
    _ continuation;    // current continuation
} state;


typedef struct {
    vector v;
    _ formals;
    _ rest;
    _ term;
    _ env;
} lambda;


typedef struct {
    vector v;
    _ term;
    _ env;
} redex;

/* All continuation frames have a parent frame, and a mark dictionary.
   The marks can be used to implement partial continuations, dynamic
   binding, ... as in

   http://people.cs.uchicago.edu/~robby/pubs/papers/icfp2007-fyff.pdf
 */
typedef struct {
    vector v;
    _ parent;
    _ marks;
} k_frame;

/* Arguments are evaluated left to right.  In retrospect it would have
   been simpler to evaluate from right to left: this makes it easier
   to use k_apply continuations for other purposes. */
typedef struct {
    k_frame k;
    _ done;   // list of values
    _ todo;   // reversed list of redexes
} k_args;

typedef struct {
    k_frame k;
    _ yes;  // non-reduced closures for the 2 branches
    _ no;
} k_if;

typedef struct {
    k_frame k;
    _ var;
    _ env;
    _ tl_slot; // state vector slot containing toplevel
} k_set;

/* Sequences are evaluated left to right.  Frame is popped before the
   last redex. */
typedef struct {
    k_frame k;
    _ todo;  
} k_seq;


// conversion from vector object -> C type
DEF_STRUCT(state,  TAG_STATE)
DEF_STRUCT(lambda, TAG_LAMBDA)
DEF_STRUCT(redex,  TAG_REDEX)

DEF_STRUCT(k_args,  TAG_K_ARGS)
DEF_STRUCT(k_if,    TAG_K_IF)
DEF_STRUCT(k_set,   TAG_K_SET)
DEF_STRUCT(k_seq,   TAG_K_SEQ)




/* SCHEME */








/* Interpreter exceptions. */
//#define SC_EX_TRY     0
//#define SC_EX_RESTART 1  /* restart from current sc->state. */
//#define SC_EX_ABORT   2  /* abort to default toplevel continuation. */
//#define SC_EX_HALT    3  /* halt leaves the interpreter loop. */
//#define SC_EX_CK      4  /* wrap up C continuation */

/* Macros valid in sc context. */
#define STATE(c,k)   sc_make_state(sc,c,k)
#define REDEX(t,e)   sc_make_redex(sc,t,e)

// #define VALUE(d)     sc_make_value(sc,d)


    
/* Toplevel evaluation */
#define EVAL(expr)    POST(_sc_top((sc*)EX, expr))

// safe cast to C struct
// object sc_raise_type_error(sc *sc, object arg_o);



// Scheme constants start at 0x100
#define MT    CONSTANT(0x100)


/* For maximal separation (no access to internal data), the VM can be
   driven using a string interface.  This function returns the output
   written to current-output-port as a string.  Its data is live until
   the next entry. */
const char *_sc_repl_cstring(sc *sc, const char *commands);

/* INIT */
sc *_sc_new(int argc, const char **argv);
void _sc_def_prims(sc *sc, prim_def *prims);

void _sc_eval_cstring(sc *sc, const char *commands); // deprecated






/* Coroutine yield to VM: exchange a string.  The returned string is
   only valid inbetween _sc_yield calls. */
const char *_sc_yield(sc *sc, const char *msg);


/* For these it is simpler to split a "start" operation into a
   "prepare" and "continue" operation, where the former sets the VM
   state and the latter simply resumes execution. */

object _sc_continue(sc *sc);  // resume execution at current machine state
void _sc_prepare(sc *sc, _ expr);  // current state = start to evaluate expr
object _sc_top(sc *sc, object expr);  /* == set_redex + continue */

console *_sc_prepare_console_server(sc *sc, const char *node, int port);



typedef struct {
    sc super;  // derive from shared sc struct

    /* Special form symbol cache */
    _ s_lambda;
    _ s_begin;
    _ s_quote;
    _ s_if;
    _ s_bang_set;
    _ s_letcc;
    
} sc_interpreter;



#endif
