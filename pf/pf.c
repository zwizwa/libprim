#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include "pf.h"
#include "px.h"
#include "pf.h_pf_prims"
#include "px.h_px_prims"

/* 
   PF: VM interpreter and Stack primitive functions.

   The code in this file is partitioned in two classes:

   pf_   linear stack words (written in terms of _px_ and px_)
   _px_  misc functions (not respecting px_ nor pf_ API) 

*/




/* INTERPRETER */

/*
      http://zwizwa.be/-/libprim/20090816-120957

      SEQ   = (SUB : SUB)            ;; `:' is graph CONS
      SUB   = PRIM | QUOTE | SEQ
 
      K     = NIL | (SUB . K)        ;; `.' is linear CONS
*/
typedef void (*pf_prim)(pf*);

void _px_run(pf *pf) {
    seq *s;
    prim *p;
    quote *q;
    box *b;
    lin *l;

    pf_prim fn = NULL;

    /* Toplevel exceptions. */
    EX->top_entries++;
    while (setjmp(pf->m.top)) {
        /* Restarts can only happen in primitives.  The rest of the
           interpreter is linear.  We re-push the IP because it has
           been popped right before execution. */
        if (pf->m.r.prim) {
            PUSH_K_NEXT(const_to_object(pf->m.r.prim));
        }
        else {
            /* Exit condition. */
            return;
        }

        /* Reset state. */
        pf->m.r.prim = NULL;
        pf->m.prim_entries = 0;
    }

  loop:
    for(;;) {
        
        /* By default, run each step in LINEAR mode (GC allocation
           switched off).

           A primitive is allowed to switch to PURE mode as long as
           there are no side effects performed between this LINEAR()
           call and the PURE() invocation in the primitive.
        */

        /* Quote linear datum (Implement the `dip' continuation.) */
        pair *rs = object_to_ldata(pf->k);
        if (unlikely(rs)) {
            PUSH_P(rs->car);
            rs->car = VOID;
            DROP_K();
        }

        /* Consume next instruction: pop K. */
        else {

            rs = object_to_lnext(pf->k);
            if (unlikely(!rs)) goto halt;
            _ ip = rs->car;
            
            /* Unpack code sequence, push K. */
            if ((s = object_to_seq(ip))) {
                DROP_K(); // (*)
                PUSH_K_NEXT(s->next);
                PUSH_K_NEXT(s->now);
            }
            /* Interpret primitive code or data. */
            else {
                /* Primitive */
                if ((p = object_to_prim(ip, &pf->m))) {
                    DROP_K(); // (*)
                    int ex;
                    fn = (pf_prim)p->fn;
                    pf->m.r.prim = p;
                    pf->m.prim_entries++;
                    switch(ex = setjmp(pf->m.r.step)) {
                    case 0:
                        /* Check memory + allow restart */
                        EX->stateful_context = 0;
                        if (gc_available(EX->gc) < 20) gc_collect(EX->gc);
                        
                        /* Disallow restart by default for primitives.
                           Non-bounded allocation needs to explictly
                           re-enable restart. */
                        EX->stateful_context = 1;
                        fn(pf);
                        fn = NULL;
                        break;
                    default:
                        pf->m.error_tag = SYMBOL("unknown-primitive-exception");
                        pf->m.error_arg = integer_to_object(ex);
                    case EXCEPT_ABORT:
                        PUSH_P(LINEARIZE_EXCEPTION(pf->m.error_arg));
                        PUSH_P(LINEARIZE_EXCEPTION(pf->m.error_tag));
                        PUSH_K_NEXT(pf->ip_abort);
                    }
                    pf->m.prim_entries--;
                }
                /* Quoted object */
                else if ((q = object_to_quote(ip))) {
                    DROP_K(); // (*)
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
                    PUSH_P(ob);
                }
                /* Result of popping an empty continuation. */
                else if (HALT == ip) {
                    DROP_K();  // (*)
                    goto halt;
                }
                /* Unknown non-seq. */
                else {
                    /* Because a type different from the cases above
                       _can_ be linear, we postponed the DROP_K()
                       until after ip was determined to be nonliner:
                       see the (*) marks above.  */
                    FROM_TO(k, p);
                    PUSH_P(SYMBOL("unknown-instruction"));
                    PUSH_K_NEXT(pf->ip_abort);
                }
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
    _px_drop(pf, &pf->p);
}
void pf_dup(pf *pf) {
    PUSH_P(_px_link(pf, TOP));
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
    EXCH(_CAR(pf->p), x->object); _DROP();
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
// this uses linear cons from EX->make_pair
void pf_read(pf *pf) {
    static port *p = NULL;
    if (!p) p = port_file_new(stdin, "<stdin>");
    PUSH_P(_ex_read(EX, p));
}

void pf_ps(pf *pf) {  // print stack
    POST_STACK(pf->p);
}
void pf_pm(pf *pf) {  // print machine
    _ex_printf(EX, "P: "); POST_STACK(pf->p);
    _ex_printf(EX, "K: "); POST(pf->k);
    _ex_printf(EX, "F: %d\n", object_to_integer(LENGTH(pf->freelist)));
}
void pf_pd(pf *pf) {  // print dict
    _ E = pf->dict;
    while (NIL != E) {
        POST(CAR(E));
        E = CDR(E);
    }
}


/* Implementing (conservative) tree transformations. 

   A tree transformer can be derived using the following manual
   compilation approach:

       1. Write down the transform in a high level dotted pair form:
          LHS -> RHS
    
       2. Following the structure in LHS, bind the nodes (and
          deconstructed dot pairs) to C lexical variables using
          type-checking casts for the pairs.  This makes sure
          exceptions happen before we mutate anything.  

       3. Rebuild the tree by re-using the pairs to create the dotted
          tree in the RHS.

   To increase readability, index the pairs from left to right as they
   appear in the textual form of LHS and RHS.

*/


/*  D = datum
    L = list
    P = parameter stack
    
      (L . (D . P)) -> ((D . L) . P)
*/
void pf_cons(pf *pf) {
    pair *dot0 = CAST(lpair, pf->p);
    pair *dot1 = CAST(lpair, dot0->cdr);
    _ L = dot0->car;
    _ D = dot1->car;
    _ P = dot1->cdr;
    pf->p     = VEC(dot1);
    dot1->car = VEC(dot0);
    dot0->car = D;
    dot0->cdr = L;
    dot1->cdr = P;
}

/*  D = datum
    L = list
    P = parameter stack

      ((D . L) . P) -> (L . (D . P))
*/
void pf_uncons(pf *pf) {
    pair *dot0 = CAST(lpair, pf->p);
    pair *dot1 = CAST(lpair, dot0->car);
    _ D = dot1->car;
    _ L = dot1->cdr;
    _ P = dot0->cdr;
    pf->p     = VEC(dot0);
    dot0->cdr = VEC(dot1);
    dot0->car = L;
    dot1->car = D;
    dot1->cdr = P;
}

/*  (A . (B . P)) -> (B . (A . P)) */
void pf_swap(pf *pf) {
    pair *dot0 = CAST(lpair, pf->p);
    pair *dot1 = CAST(lpair, dot0->cdr);
    _ A = dot0->car;
    _ B = dot1->car;
    dot0->car = B;
    dot1->car = A;
}


void pf_output(pf *pf) {
    PUSH_P(_px_link(pf, pf->output));
}
void pf_stack(pf *pf) {
    _px_need_free(pf);
    _ p = pf->p; pf->p = NIL;
    PUSH_P(p);
}
void pf_print_error(pf *pf) {
    _ex_printf(EX, "ERROR: ");
    if (NIL == pf->p) PUSH_P(SYMBOL("unknown")); 
    _WRITE();
    if (NIL == pf->p) PUSH_P(VOID); 
    if (TOP == VOID) { 
        _DROP(); 
    }
    else { 
        _ex_printf(EX, ": "); 
        _WRITE(); 
    }
    _ex_printf(EX, "\n");
}
void pf_abort_repl(pf *pf) {
    _px_unlink(pf, pf->k);
    pf->k = LINEAR_NEXT(pf->ip_repl, NIL);
}

void pf_nop(pf *pf) {}

/* About the usage of PURE() */

/* Since we have a non-rentrant interpreter with mutable state, this
   is a bit less problematic than the EX/SC case. */
void pf_gc(pf *pf) {
    pf->m.r.prim = object_to_prim(pf->ip_nop, EX);  // don't restart pf_gc() !
    gc_collect(GC); // does not return
}
void pf_gc_test(pf *pf) {
    _GC_STAT();
    _px_alloc_cells(pf, 2000);
}


/* Primitives in terms of expressions.  Note that an upper case name
   like _XXX() is short for a stack word pf_xxx(pf *).  */
void pf_write(pf *pf)     { WRITE(TOP); _DROP(); }
void pf_space(pf *pf)     { _ex_printf(EX, " "); }
void pf_p(pf *pf)         { _WRITE(); _SPACE(); }
void pf_cr(pf *pf)        { _ex_printf(EX, "\n"); }
void pf_post(pf *pf)      { POST(TOP); _DROP(); }
void pf_trap(pf *pf)      { TRAP(); }
void pf_reverse(pf *pf)   { _TOP = BANG_REVERSE(TOP); }
void pf_add1(pf *pf)      { _TOP = ADD1(TOP); }
void pf_find(pf *pf)      { _TOP = FIND(pf->dict, TOP); }

void pf_add(pf *pf)       { _TOP = ADD(TOP, SECOND); _SWAP(); _DROP(); }

void pf_display(pf *pf)   { px_display(pf, TOP); _DROP(); } 

void pf_words(pf *pf) {
    _ d = pf->dict;
    while (NIL != d) {
        px_write(pf, CAAR(d)); 
        _SPACE();
        d = CDR(d);
    }
    _CR();
}


void pf_interpret(pf *pf) {
    _ v = TOP;
    /* Perform linearly if possible. */
    if (object_to_symbol(v, EX)) {
        _ ob = FIND(pf->dict, v);
        if (FALSE == ob) {_DROP(); ERROR_UNDEFINED(v);}
        _TOP = ob;
        _RUN();
        return;
    }
    if (GC_INTEGER == GC_TAG(v)) {
        return;
    }
    if (object_to_lpair(v)) {
        if (pf->s_quote == _CAR(v)) { 
            pair *dp = CAST(lpair, _CDR(v));
            _ datum = dp->car;
            dp->car = VOID;
            _DROP();
            PUSH_P(datum);
            return;
        }
        /* Compile quotation (nonlinearly). 
           FIXME: this could be linear.
         */
        _TO_NL();
        _NL_COMPILE();
        return;
    }
    if (EOF_OBJECT == v) {
        return pf_bye(pf);
    }
    /* Datum: leave intact. */
}



/* Note that the compiler uses nonlinear data structures.  When
   entering the compiler from within PF, all data needs to be
   converted to nonlinear form first.
*/


/* FIXME: properly switch mode! */
void pf_make_loop(pf *pf) { 
    _TOP = MAKE_LOOP(TOP);
}

void pf_nl_compile(pf *pf) { 
    _TOP = COMPILE_PROGRAM(TOP);
}

void pf_nl_definitions(pf *pf) { 
    px_define(pf, TOP); 
    _DROP();
}

void pf_define(pf *pf) {
    _ var = TOP;
    _ val = SECOND;
    pf->dict = ENV_DEF(pf->dict, var, val);
}

void pf_to_nl(pf *pf) {
    _ ob = TOP;
    _ nl = COPY_TO_GRAPH(ob);
    _DROP();
    PUSH_P(nl);
}





static inline int is_nlcode(_ ob, ex* ex) {
    return (object_to_prim(ob, ex) ||
            object_to_quote(ob) ||
            object_to_seq(ob));
}
static inline int is_lcode(_ ob, ex* ex) {
    return (object_to_lnext(ob) ||
            object_to_ldata(ob) ||
            (NIL == ob));
}


void pf_run(pf *pf){ 
    _ v = TOP;
    if (is_lcode(v, EX)) {

        /* This makes linear lists behave as programs.  Note that this
           pushes a partial continuation: it does not replace a full
           one! */
        pf->k = BANG_LINEAR_COMPOSE(v, pf->k);
        _TOP = VOID; _DROP();
    }
    else if (is_nlcode(v, EX)) {
        PUSH_K_NEXT(v);
        _DROP();
    }
    else TYPE_ERROR(v);
}
void pf_dip(pf *pf) {
    _SWAP();
    PUSH_K_DATA(TOP);
    _TOP = VOID;
    _DROP();
    _RUN();
}

void pf_to_k_data(pf *pf) {
    FROM_TO(p, k);
    vector_reset_flags(object_to_vector(pf->k), TAG_LDATA);
}
void pf_to_k_next(pf *pf) {
    FROM_TO(p, k);
    vector_reset_flags(object_to_vector(pf->k), TAG_LNEXT);
}

/* MAP + EACH, written in CPS */
void pf_map_next(pf *pf) {
    pair *p0 = CAST(ldata, pf->k);    _ *out = &(p0->car);
    pair *p1 = CAST(ldata, p0->cdr);  _ *in  = &(p1->car);
    pair *p2 = CAST(ldata, p1->cdr);  _ code = p2->car;

    _px_from_to(pf, &pf->p, out);  // collect result
    if (NIL == (*in)) {
        PUSH_P(*out); *out = VOID;
        DROP_K();
        DROP_K();
        DROP_K();
        _REVERSE();
        return;
    }
    _px_from_to(pf, in, &pf->p);
    PUSH_K_NEXT(pf->ip_map_next);
    PUSH_P(_px_link(pf, code));
    _RUN();
}
// ( lst code -- ) 
void pf_map(pf *pf) {
    _ code = TOP; _TO_K_DATA();
    _ lst  = TOP;
    if (NIL == lst) {
        DROP_K();
        return;
    }
    _TO_K_DATA();
    PUSH_K_DATA(NIL); // result
        
    _px_from_to(pf, &(_CADR(pf->k)), &pf->p);
    PUSH_K_NEXT(pf->ip_map_next);
    PUSH_P(_px_link(pf, code));
    _RUN();
}


void pf_each_next(pf *pf) {
    pair *p1 = CAST(ldata, pf->k);    _ *in  = &(p1->car);
    pair *p2 = CAST(ldata, p1->cdr);  _ code = p2->car;

    if (NIL == (*in)) {
        DROP_K();
        DROP_K();
        return;
    }
    _px_from_to(pf, in, &pf->p);
    PUSH_K_NEXT(pf->ip_each_next);
    PUSH_P(_px_link(pf, code));
    _RUN();
}

void pf_each(pf *pf) {
    _ code = TOP; _TO_K_DATA();
    _ lst  = TOP;
    if (NIL == lst) {
        DROP_K();
        _DROP();
        return;
    }
    _TO_K_DATA();
        
    _px_from_to(pf, &(_CAR(pf->k)), &pf->p);
    PUSH_K_NEXT(pf->ip_each_next);
    PUSH_P(_px_link(pf, code));
    _RUN();
}


void pf_domap(pf *pf) {
    _ fn = TOP;
}




/* Project linear/nonlinear code -> linear code */
void pf_to_lcode(pf *pf) {
    _ ob = TOP;
    if (is_nlcode(ob, EX)) {
        _TOP = LINEAR_NEXT(ob, NIL);
        return;
    }
    else if (is_lcode(ob, EX)) {
        return;
    }
    TYPE_ERROR(ob);
}
/* Quote datum as linear code */
void pf_abstract(pf *pf) {
    _TOP = LINEAR_DATA(TOP, NIL);
}

void pf_lcompose(pf *pf) {
    _ first = SECOND;
    _ next  = TOP;
    _ composed = BANG_LINEAR_COMPOSE(first, next);
    _TOP = VOID; _DROP();
    _TOP = composed;
}


/* Delimited continuations. */
void pf_prompt_tag(pf *pf) { 
/* This primitive's continuation frame is used as a marker. */
}
void pf_reset(pf *pf) {
    PUSH_K_NEXT(pf->ip_prompt_tag);
    _RUN();
}

/* transfer-upto-prompt ( lin -- lin+ )

   Transfer current marked continuation segment to the linear code
   object, up to but not including the prompt. */

static inline pair *_px_kframe(pf *pf, _ ob) {
    pair *p = object_to_ldata(ob);
    if (!p) p = object_to_lnext(ob);
    if (unlikely(!p)) { ERROR("missing-prompt", VOID); }
    return p;
}

// FIXME: restarts
void pf_transfer_upto_prompt(pf *pf) {
    _ tail = TOP; 
    if (!is_lcode(tail, EX)) TYPE_ERROR(tail);
    /* Move cells from pk->k to k upto the prompt tag. */
    _ *pk = &(_CAR(pf->p));
    pair *p = _px_kframe(pf, pf->k);
    while(p->car != pf->ip_prompt_tag) {
        *pk = pf->k;
        pf->k = p->cdr;
        pk = &_CDR(*pk);
        *pk = tail; // keep data consistency: next might throw error
        p = _px_kframe(pf, pf->k);
    }
}

// FIXME: GC restarts
void pf_control(pf *pf) {
    _ code = TOP;
    _TOP = NIL;
    _TRANSFER_UPTO_PROMPT();
    PUSH_P(code);
    _RUN();
}
void pf_shift(pf *pf) {
    _ code = TOP;
    _TOP = LINEAR_NEXT(pf->ip_prompt_tag, NIL);
    _TRANSFER_UPTO_PROMPT();
    PUSH_P(code);
    _RUN();
}


/* Full continuations.  These do not use marking and will capture the
   entire state as linear code by transferring the parameter stack to
   the continuation. */
void pf_call_with_cc(pf *pf) {
    _ fn = TOP;
    _ new_k = LINEAR_NEXT(fn, NIL);
    _TOP = pf->k;
    pf->k = new_k;
}

void pf_bang_lunrun_and_compose(pf *pf) {
    _ k = TOP;  
    if (!(is_lcode(k, EX))) {
        TYPE_ERROR(k);
    }
    _TOP = VOID; _DROP();
    while (NIL != pf->p) {
        k = LINEAR_DATA(_CAR(pf->p), k);
        _CAR(pf->p) = VOID;
        _DROP();
    }
    PUSH_P(k);
}

void pf_bang_cc(pf *pf) {
    _ v = TOP;
    _px_unlink(pf, pf->k);
    pf->k = MOVE(_TOP, VOID);
    _DROP();
}
void pf_bang_abort(pf *pf) {
    _ v = TOP;
    _px_unlink(pf, pf->k);
    pf->k = v;
    _TOP = VOID; _DROP();
}

/* Exit + make sure all the finalizers get called. */
void pf_bye(pf *pf) {
    pf->p = NIL;
    pf->k = NIL;
    pf->freelist = NIL;
    pf->dict = NIL;
    pf->output = NIL;
    pf->ip_abort = HALT;
    pf->ip_repl  = HALT;
    pf->ip_nop   = HALT;
    pf->ip_map_next = HALT;
    pf->ip_each_next = HALT;
    pf->m.r.prim = NULL; // after GC: exit VM instead of prim restart
    gc_collect(GC); // does not return
}



/* GC+SETUP */

#define MARK(reg) pf->reg = gc_mark(GC, pf->reg)

void pf_gc_stat(pf *pf) {
    long used = pf->m.gc->current_index;
    long free = pf->m.gc->slot_total - used;
    _ex_printf(EX, ";; gc %d:%d\n", (int)used, (int)free);
}

static void _px_mark_roots(pf *pf, gc_finalize fin) {
    MARK(p);
    MARK(k);
    MARK(freelist);
    MARK(output);
    MARK(ip_abort);
    MARK(ip_map_next);
    MARK(ip_each_next);
    MARK(ip_repl);
    MARK(ip_nop);
    MARK(dict);
    if (fin) { 
        fin(GC); 
        _GC_STAT();
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
    pf *pf = calloc(1, sizeof(*pf));

    // Garbage collector.
    pf->m.gc = 
        gc_new(10000, pf, 
               (gc_mark_roots)_px_mark_roots,
               (gc_overflow)_ex_overflow);
    
    // Leaf types.
    pf->m.p = malloc(sizeof(*(pf->m.p)));
    // TYPES->ck_type = ck_type();
    TYPES->symbol_type = symbol_type();
    TYPES->port_type = port_type();
    TYPES->bytes_type = bytes_type();
    TYPES->prim_type = (void*)0xF001; 
    TYPES->rc_type = (void*)0xF002; 

    // Read/Write delegate
    pf->m.write = (ex_m_write)px_write;
    pf->m.port  = (_ex_m_port)_px_port;
    pf->m.make_string = (_ex_m_make_string)_px_make_string;
    pf->m.make_qstring = (_ex_m_make_string)_px_make_qstring;
    pf->m.make_pair = (ex_m_make_pair)px_linear_cons;

    // Symbol cache
    pf->s_underflow = SYMBOL("underflow");
    pf->s_quote     = SYMBOL("quote");
    pf->s_var       = SYMBOL("var");

    // Machine state
    pf->p = NIL;
    pf->freelist = _px_alloc_cells(pf, 1000);
    pf->dict = NIL;
    pf->k = NIL;
    pf->ip_abort = HALT;
    pf->ip_nop = HALT;

    // Exceptions
    pf->m.top_entries = 0;
    pf->m.prim_entries = 0;

    // Stdout
    pf->output = _px_make_port(pf, stdout, "stdout");

    // Bootstrap repl and abort code.
    _px_def_all_prims(pf);
    _ rep = 
        SEQ(QUOTE(LIN(_px_make_string(pf, "> "))),
            SEQ(WORD("display"),
                SEQ(WORD("read"), 
                    WORD("interpret"))));
    _ repl = MAKE_LOOP(rep);

    _ definitions = SEQ(WORD(">nl"), WORD("nl-definitions"));
    pf->dict = ENV_DEF(pf->dict, SYMBOL("definitions"), definitions);
    pf->dict = ENV_DEF(pf->dict, SYMBOL("rep"), rep);
    pf->dict = ENV_DEF(pf->dict, SYMBOL("repl"), repl);
    pf->ip_repl  = repl;
    pf->ip_abort = SEQ(WORD("print-error"), WORD("abort-repl"));
    pf->ip_prompt_tag = WORD("prompt-tag");
    pf->ip_nop = WORD("nop");
    pf->ip_map_next = WORD("map-next");
    pf->ip_each_next = WORD("each-next");
    // Highlevel bootstrap
    _px_interpret_list(pf, _ex_boot_load(EX, "boot.pf"));
    return pf;
}

/* Top level evaluators. */
void _px_interpret_list(pf *pf, _ l_expr){
    PUSH_P(l_expr);
    l_expr = NIL;
    _TO_NL();
    _NL_COMPILE();
    _RUN();
    _px_run(pf);
}
/* Find and run.  This is linear if the referenced code is. */
void _px_interpret_symbol(pf *pf, _ sym) {
    PUSH_K_NEXT(FIND(pf->dict, sym));
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
