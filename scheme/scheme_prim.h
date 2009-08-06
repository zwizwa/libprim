#ifndef _SCHEME_PRIM_H_
#define _SCHEME_PRIM_H_
object sc_is_bool (sc *sc, object o);
object sc_is_integer(sc *sc, object o);
object sc_is_zero (sc *sc, object o);
object sc_is_symbol(sc *sc, object o);
object sc_is_prim(sc *sc, object o);
object sc_is_null(sc *sc, object o);
object sc_is_vector(sc *sc, object o);
object sc_is_pair(sc *sc, object o);  
object sc_is_lambda(sc *sc, object o); 
object sc_is_closure(sc *sc, object o);
object sc_is_state(sc *sc, object o); 
object sc_is_frame(sc *sc, object o);
object sc_make_pair(sc *sc, object car, object cdr);
object sc_make_state(sc *sc, object C, object K);
object sc_make_closure(sc *sc, object C, object K);
object sc_make_lambda(sc *sc, object car, object cdr);
object sc_make_frame(sc *sc, object v, object c, object l);
object sc_make_syntax(sc *sc, object datum);
object sc_error(sc *sc, object sym_o, object o);
object sc_unsafe_assert(sc *sc, sc_1 predicate, object o);
object sc_make_vector(sc *sc, object slots);
object sc_reverse(sc *sc, object lst);
object sc_length(sc *sc, object lst);
object sc_list_to_vector(sc *sc, object lst);
object sc_find(sc *sc, object E, object var);
object sc_find_toplevel(sc *sc, object var);
object sc_is_list(sc *sc, object o);
object sc_write(sc *sc, object o);
object sc_post(sc* sc, object o);
object sc_close_args(sc *sc, object lst, object E);
object sc_interpreter_step(sc *sc, object o_state);
object sc_datum_to_state(sc *sc, object expr);
object sc_trap(sc *sc);
object sc_gc(sc* sc);
object sc_setvar(sc* sc, object var, object val);
object sc_car(sc *sc, object o);
object sc_cdr(sc *sc, object o);

#endif
