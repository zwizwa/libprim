// Hack it on top of SC

#define SC_NEW_VM
#include <sc/scheme.c>
#include <mz/vm.h_prims>

static prim_def vm_prims[] = vm_table_init;

/* Continuation frames.

   Note: - k_code is NOT a prim struct but a pointer to machine code.

         - no type checking in VM (make sure internal data is properly
           shielded from scheme access)

         - code needs to be aligned to word boundaries to make sure
           the GC treats the pointer as a constant. 
*/

typedef void (*k_code)(sc *sc, _ value, void *frame);
typedef struct {
    vector v;
    k_code tag;  // VM code point to jump to
    _ parent;   // parent continuation frame
    // _ marks;
} kf_base;



/* CONTINUATION FRAMES */

/* VM ops creating continuation frames. */
#define CHECK_ALIGNED(x) { if(((void*)x) != GC_POINTER((object)x)) TRAP(); }

static kf_base *_sc_extend_kf_base(sc *sc, int slots, void *tag) {
    kf_base *kf = (kf_base*)gc_alloc(EX->gc, slots);
    kf->parent = sc->k;
    sc->k = vector_to_object((vector*)kf);
    kf->tag = (k_code)tag;
    CHECK_ALIGNED(tag);
    return kf;
}

#define EXTEND_K(type, name) \
    type *name = (type*)_sc_extend_kf_base\
        (sc, ((sizeof(type))/sizeof(_))-1, _sc_##type);

/* All other instructions create continuation frames. */
typedef struct {
    kf_base f;
    _ yes;
    _ no;
} kf_if;
void _sc_kf_if(sc *sc, _ v, kf_if *f) {
    sc->c = (FALSE == v) ? f->no : f->yes;
}
typedef kf_base kf_halt;
void _sc_kf_halt(sc *sc, _ v) {
    ERROR("vm-halt", v);
}



/* VM OPCODES. */
typedef void (*vm_code)(sc *sc, _ args);

/* Each instruction modifies the current state in-place, possibly
   extending the continuation frame.  These are defined close to each
   other to keep the code cache footprint small. */
void _sc_vm_if(sc *sc, _ args) {
    EXTEND_K(kf_if, f);
    sc->c  = _CAR(args); args = _CDR(args);  // eval condition first
    f->yes = _CAR(args); args = _CDR(args);  // save 2 possible code paths
    f->no  = _CAR(args);
}
void _sc_vm_lit(sc *sc, _ v) {
    kf_base *f = (kf_base*)GC_POINTER(sc->k); // unsafe
    sc->k = f->parent;                        // pop frame
    k_code fn = f->tag;
    fn(sc, v, f);
}
void _sc_vm_prim(sc *sc, _ args) {
    prim *p = (prim *)GC_POINTER(_CAR(args));
    args = _CDR(args);
    _ value = _sc_call(sc, prim_fn(p), prim_nargs(p), args);
    _sc_vm_lit(sc, value);
}
_ sc_vm_continue(sc *sc) {

    // piggyback on mother vm for exceptions & abort
    for(;;) {
        /* Run next opcode.  
           
           Machine primitives are implemented using the same data type
           as C primitives.

           Note however that primitives that occur in the instruction
           stream need to modify the machine state in-place: their
           return value is ignored.  An ordinary primtive that doesn't
           touch sc->c will cause an infinite loop.

           Don't perform type checking.  That's the task of the
           compiler.
        */
        
        vm_code fn = (vm_code)_CAR(sc->c);
        _ args     = _CDR(sc->c);
        fn(sc, args);
    }
}



/* Reflection.  These C functions are visible from the Scheme side.  */

#define OP(name) { return const_to_object(_sc_vm_##name); }

_ sc_op_if(sc *sc)   OP(if);
_ sc_op_lit(sc *sc)  OP(lit);
_ sc_op_prim(sc *sc) OP(prim);

_ sc_vm_init(sc *sc, _ c) {
    sc->e = NIL;
    sc->k = NIL;
    sc->c = c;
    EXTEND_K(kf_halt, f);
    return VOID;
}

int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, (const char**)argv);
    _sc_def_prims(sc, vm_prims);
    _sc_continue(sc);
    return 0;
}
