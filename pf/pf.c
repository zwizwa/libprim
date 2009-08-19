#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include "pf.h"
#include "px.h"
#include "pf.h_pf_prims"
#include "px.h_px_prims"

/* 
   PF: VM interpreter and Stack primitive functions

   The code in this file is partitioned in two classes:

   pf_   linear stack words (written in terms of _px_ and px_)
   _px_  misc functions (not respecting px_ nor pf_ API) 

*/




/* INTERPRETER */

/*
      http://zwizwa.be/-/libprim/20090816-120957

      SEQ   = (SUB : SUB)            ;; `:' is graph CONS
      SUB   = PRIM | QUOTE | SEQ
 
      RS    = NIL | (SUB . RS)       ;; `.' is linear CONS
*/
typedef void (*pf_prim)(pf*);

#define PUSH_RS(x)  pf->rs = LINEAR_CONS((x), pf->rs)
#define DROP_RS()   _px_drop(pf, &pf->rs)

void _px_run(pf *pf) {
    seq *s;
    prim *p;
    quote *q;
    box *b;
    lin *l;

    /* Toplevel exceptions. */
    EX->top_entries++;
    while (setjmp(pf->m.top)) {
        pf->m.r.prim = NULL;
        pf->m.prim_entries = 0;
    }

  loop:
    /* Interpeter loop. */
    for(;;) {

        /* Consume next instruction from RS. */
        pair *rs = object_to_lpair(pf->rs);
        if (unlikely(!rs)) goto halt;
        _ ip = rs->car;
        DROP_RS();

        /* Unpack code sequence, push RS. */
        if ((s = object_to_seq(ip))) {
            PUSH_RS(s->next);
            PUSH_RS(s->now);
        }
        /* Interpret primitive code or data and pop RS. */
        else {
            /* Primitive */
            if ((p = object_to_prim(ip, &pf->m))) {
                pf_prim fn = (pf_prim)p->fn;
                int ex;
                pf->m.r.prim = p;
                pf->m.prim_entries++;
                switch(ex = setjmp(pf->m.r.step)) {
                case 0:
                    fn(pf);
                    break;
                default:
                    _px_push(pf, SYMBOL("unknown-exception"));
                case EXCEPT_ABORT:
                    // TAG + ARG are NONLINEAR
                    _px_push(pf, COPY_FROM_GRAPH(pf->m.error_arg));
                    _px_push(pf, COPY_FROM_GRAPH(pf->m.error_tag));
                    PUSH_RS(pf->ip_abort);
                }
                pf->m.prim_entries--;
            }
            /* Quoted object */
            else if ((q = object_to_quote(ip))) {
                _ ob;
                if ((l = object_to_lin(q->object))) {
                    /* Unpack + link linear objects */
                    ob = _px_link(pf, l->object);
                }
                else {
                    /* All other objects behave as constants to the
                       linear memory manager. */
                    ob = q->object;
                }
                _px_push(pf, ob);
            }
            /* The empty program. */
            else if (NOP == ip) {
            }
            /* Result of popping an empty continuation. */
            else if (HALT == ip) {
                goto halt;
            }
            /* Unknown non-seq. */
            else {
                _px_push(pf, COPY_FROM_GRAPH(ip));
                _px_push(pf, SYMBOL("unknown-instruction"));
                PUSH_RS(pf->ip_abort);
            }
        }
    }

  halt:
    /* Return to caller. */
    EX->top_entries--;
    return;

}

/* PRIMITIVES */

// stacks + boxes  (linear memory management)
void pf_drop(pf *pf) {
    _px_drop(pf, &pf->ds);
}
void pf_dup(pf *pf) {
    _px_push(pf, _px_link(pf, TOP));
}
void pf_to_dict(pf *pf) {
    _ ob = COPY_TO_GRAPH(TOP);
    pf->dict = ex_cons(&pf->m, ob, pf->dict);
    _DROP();
}
void pf_fetch_(pf *pf) {
    aref *x = object_to_box(TOP);
    _TOP = _px_link(pf, x->object);
}
void pf_bang_(pf *pf) {
    aref *x = object_to_box(TOP); _DROP();
    EXCH(_CAR(pf->ds), x->object); _DROP();
}
void pf_fetch_from_(pf *pf) {
    aref *x = object_to_box(TOP);
    _TOP = MOVE(x->object, VOID);
}
void pf_exchange(pf *pf) {
    aref *x = object_to_box(TOP);
    _DROP();
    EXCH(x->object, _TOP);
}
void pf_read(pf *pf) {
    /* FIXME: make sure read has a low probability to restart, which
     * would mess up its state. */
    // pf_gc(pf); 
    
    port p;
    p.stream = stdin;
    /* FIXME: read needs to produce linear data */
    _px_push(pf, COPY_FROM_GRAPH(_ex_read(EX, &p)));
}

void pf_ps(pf *pf) {  // print stack
    POST_STACK(pf->ds);
}
void pf_pm(pf *pf) {  // print machine
    _ex_printf(EX, "P: "); POST_STACK(pf->ds);
    _ex_printf(EX, "R: "); POST_STACK(pf->rs);
    _ex_printf(EX, "F: %d\n", object_to_integer(LENGTH(pf->free)));
}
void pf_pd(pf *pf) {  // print dict
    _ E = pf->dict;
    while (NIL != E) {
        POST(CAR(E));
        E = CDR(E);
    }
}


void pf_output(pf *pf) {
    _px_push(pf, _px_link(pf, pf->output));
}
void pf_stack(pf *pf) {
    _px_need_free(pf);
    FROM_TO(free, rs);
    _CAR(pf->rs) = MOVE(pf->ds, NIL);
    FROM_TO(rs, ds);
}
void pf_print_error(pf *pf) {
    _ex_printf(EX, "ERROR: ");
    if (NIL == pf->ds) _px_push(pf, SYMBOL("unknown")); 
    _WRITE();
    if (NIL == pf->ds) _px_push(pf, VOID); 
    if (TOP == VOID) { 
        _DROP(); 
    }
    else { 
        _ex_printf(EX, ": "); 
        _WRITE(); 
    }
    _ex_printf(EX, "\n");
}

/* Since we have a non-rentrant interpreter with mutable state, this
   is a bit less problematic than the EX/SC case. */
void pf_gc(pf *pf) {
    gc_collect(GC); // does not return
}

/* Primitives in terms of expressions.  Note that an upper case name
   like _XXX() is short for a stack word pf_xxx(pf *).  */
void pf_write(pf *pf)   { WRITE(TOP); _DROP(); }
void pf_p(pf *pf)       { _WRITE(); _ex_printf(EX, " "); }
void pf_cr(pf *pf)      { _ex_printf(EX, "\n"); }
void pf_post(pf *pf)    { POST(TOP); _DROP(); }
void pf_trap(pf *pf)    { TRAP(); }
void pf_reverse(pf *pf) { _TOP = BANG_REVERSE(TOP); }
void pf_add1(pf *pf)    { _TOP = ADD1(TOP); }

void pf_to_r(pf *pf)    { FROM_TO(ds, rs); }
void pf_from_r(pf *pf)  { FROM_TO(rs, ds); }

// This won't take programs.
void pf_interpret(pf *pf) {
    _ v = TOP;
    if (object_to_symbol(v, EX)) {
        _ ob = FIND(pf->dict, v);
        if (FALSE == ob) {_DROP(); ERROR_UNDEFINED(v);}
        _TOP = ob;
        _RUN();
    }
}



/* Note that the compiler uses nonlinear data structures.  When
   entering the compiler from withing PF, all data needs to be
   converted to nonlinear form first.

   FIXME: this needs a proper check to make sure the data really is
   nonlinear, before it can be taken out of the linear stack.
*/

_ _px_pop_to_graph(pf *pf) {
    _ ob = TOP;
    if (object_to_lpair(ob) || object_to_rc(ob, EX)) {
        ob = COPY_TO_GRAPH(ob);
    }
    _DROP();
    return ob;
}
#define POP_TO_GRAPH _px_pop_to_graph(pf)

void pf_define(pf *pf)  { 
    px_define(pf, POP_TO_GRAPH);
}
void pf_compile(pf *pf) { 
    _px_push(pf, COMPILE_PROGRAM(POP_TO_GRAPH));
}
void pf_run(pf *pf){ 
    _ v = POP_TO_GRAPH;
    PUSH_RS(v);
}
void pf_make_loop(pf *pf) {
    _px_push(pf, MAKE_LOOP(POP_TO_GRAPH));
}


/* Make sure all the finalizers get called. */
void pf_bye(pf *pf) {
    pf->ds = NIL;
    pf->rs = NIL;
    pf->free = NIL;
    pf->dict = NIL;
    pf->output = NIL;
    pf->ip_abort = HALT;
    // pf->ip = HALT;
    pf_gc(pf); // does not return
}



/* GC+SETUP */

#define GC_DEBUG _ex_printf(EX, ";; %d\n", (int)GC->current_index)
#define MARK(reg) pf->reg = gc_mark(GC, pf->reg)
static void _px_mark_roots(pf *pf, gc_finalize fin) {
    printf(";; gc\n");
    MARK(ds);
    MARK(rs);
    MARK(free);
    MARK(output);
    // MARK(ip);
    MARK(ip_abort);
    MARK(dict);
    if (fin) { 
        fin(GC); 
        long used = GC->current_index;
        long free = GC->slot_total - used;
        _ex_printf(EX, ";; gc %d:%d\n", (int)used, (int)free);
        _ex_restart(EX); 
    }
    else return;  // we're in gc_grow() -> return
}

static _ _px_prim(pf* pf, pf_prim fn, _ name) {
    prim *p = malloc(sizeof(*p));
    p->type = TYPES->prim_type;
    p->fn = fn;
    p->nargs = 0;
    p->var = name;
    return const_to_object(p);
}

#define PRIM(fn)  _px_prim(pf, fn)

static prim_def pf_prims[] = _pf_table_init;
static void _px_def_prims(pf *pf, prim_def *prims) {
    prim_def *prim;
    for (prim = prims; prim->name; prim++) {
        _ var = SYMBOL(prim->name);
        pf->dict = ENV_DEF(pf->dict,
                           var,
                           _px_prim(pf, prim->fn, var));
    }
}
static void _px_def_all_prims(pf *pf) {
    _px_def_prims(pf, pf_prims);
}

_ _px_word(pf* pf, const char *str) {
    _ var = SYMBOL(str);
    _ rv = FIND(pf->dict, var);
    if (FALSE == rv) ERROR_UNDEFINED(var);
    return rv;
}

#define WORD(str) _px_word(pf, str)

void _px_load_lib(pf *pf);

pf* _px_new(void) {
    pf *pf = malloc(sizeof(*pf));

    // Garbage collector.
    GC = gc_new(10000, pf, 
                (gc_mark_roots)_px_mark_roots,
                (gc_overflow)_ex_overflow);

    // Leaf types.
    pf->m.p = malloc(sizeof(*(pf->m.p)));
    TYPES->ck_type = ck_class_new();
    TYPES->symbol_type = symbol_class_new(1000);
    TYPES->port_type = port_class_new();
    TYPES->prim_type = (void*)0xF001; 
    TYPES->rc_type = (void*)0xF002; 

    // Write delegate
    pf->m.write = (ex_write_method)px_write;
    pf->m.port  = (_ex_port_method)_px_port;

    // Symbol cache
    pf->s_underflow = SYMBOL("underflow");
    pf->s_quote     = SYMBOL("quote");
    pf->s_var       = SYMBOL("var");

    // Machine state
    pf->ds = NIL;
    pf->free = NIL;
    pf->dict = NIL;
    pf->rs = NIL;
    // pf->ip = HALT;
    pf->ip_abort = HALT;

    // Exceptions
    pf->m.top_entries = 0;
    pf->m.prim_entries = 0;

    // Stdout
    pf->output = _px_make_port(pf, stdout, "stdout");

    // Bootstrap repl and abort code.
    _px_def_all_prims(pf);
    _ rep = SEQ(WORD("read"), WORD("interpret"));
    _ repl = MAKE_LOOP(rep);
    pf->dict = ENV_DEF(pf->dict, SYMBOL("rep"), rep);
    pf->dict = ENV_DEF(pf->dict, SYMBOL("repl"), repl);
    pf->ip_abort = SEQ(WORD("print-error"), repl);
                       

    // Highlevel bootstrap
    _px_interpret_list(pf, _ex_boot_load(EX, "boot.pf"));
    return pf;
}

/* Top level evaluator.  This takes a (read-only) s-expression,
   compiles it to code (this performs allocation from GC pool -- top
   eval is not linear), and executes this code until machine halt.  */
void _px_interpret_list(pf *pf, _ nl_expr){
    PUSH_RS(COMPILE_PROGRAM(nl_expr));
    _px_run(pf);
}
/* Find and run.  This is linear if the referenced code is. */
void _px_interpret_symbol(pf *pf, _ sym) {
    PUSH_RS(FIND(pf->dict, sym));
    _px_run(pf);
}
void _px_command(pf *pf, const char *str) {
    _px_interpret_symbol(pf, SYMBOL(str));
}

int main(int argc, char **argv) {
    pf *pf = _px_new();
    _px_run(pf);
    return 0;
}
