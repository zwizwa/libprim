// -*- c -*-
DEF("null?", sc_is_null, 1);
DEF("zero?", sc_is_zero, 1);

// sc_is_bool
// sc_is_integer
// sc_is_zero
// sc_unsafe_assert
// sc_is_symbol
// sc_is_prim
// sc_is_null
// sc_is_vector
// sc_is_pair
// sc_is_lambda
// sc_is_closure
// sc_is_state
// sc_is_frame
// sc_make_pair
// sc_make_state
// sc_make_closure
// sc_make_lambda
// sc_make_frame
// sc_error
DEF("write", sc_write, 1);
// sc_make_vector
// sc_reverse
// sc_length
// sc_list_to_vector
// sc_find
// sc_find_toplevel
// sc_is_list
DEF("post", sc_post, 1);
// sc_close_args
// sc_interpreter_step
DEF("cons", sc_make_pair, 2);
DEF("gc",sc_gc, 0);

