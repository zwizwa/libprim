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
// sc_make_state
// sc_make_closure
// sc_make_lambda
// sc_make_frame
DEF("make-vector", sc_make_vector, 2);
// sc_error
DEF("write", sc_write, 1);
// sc_make_vector
// sc_reverse
DEF("length", sc_length, 1);
DEF("list->vector", sc_list_to_vector, 1);
// sc_find
DEF("find-toplevel", sc_find_toplevel, 1);
DEF("find-toplevel-macro", sc_find_toplevel_macro, 1);
// sc_is_list
DEF("post", sc_post, 1);
// sc_close_args
// sc_interpreter_step
DEF("cons", sc_make_pair, 2);
DEF("gc", sc_gc, 0);
DEF("def-toplevel!", sc_def_toplevel, 2);
DEF("def-toplevel-macro!", sc_def_toplevel_macro, 2);
DEF("car", sc_car, 1);
DEF("cdr", sc_cdr, 1);
DEF("take-vector", sc_take_vector, 2);



