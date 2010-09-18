/* Both SC (Scheme) and PF (Concatenative language with linear core
   memory) use primitives from this expression language EX, which is
   essentially C with dynamic type checking and GC.

   Note that these only work inside 
     - setjump(ex->top) for GC restarts
     - setjump(ex->r.step) for primitive exceptions.
*/

/*
  FIXME: CONS in this file should sometimes be nonlinear cons (both
  for SC and PF) and somtimes linear cons (for PF).

 */

#include "config.h"
#include <leaf_posix/channel.h>
#include <ex/ex.h>
#include <ex/ex.h_prims>
#include <ex_posix/ex_posix.h_prims>


DECL_TYPE(channel)


/* PRIMITIVES */
#define EX ex

_ static _ex_fd_open(ex *ex, int fd, const char *cmode, const char *name) {
    return _ex_leaf_to_object(ex, port_file_new(fdopen(fd, cmode), name));
}

/* Returns a pair (input . output) of ports. */
_ ex_tcp_connect(ex *ex, _ host, _ port) {
    char *hostname  = CAST(cstring, host);
    int port_number = CAST_INTEGER(port);
    int fd;
    if (-1 == (fd = fd_socket(&ex->l, hostname, port_number, 0))) {
        ERROR("invalid", CONS(host, CONS(port, NIL)));
    }
    char name[20 + strlen(hostname)];
    sprintf(name, "I:%s:%d", hostname, port_number);
    _ in  = _ex_fd_open(ex, fd, "r", name); name[0] = 'O';
    _ out = _ex_fd_open(ex, fd, "w", name);
    return CONS(in, out);
}

/* Returns a unix FILE DESCRIPTOR!  This can then be passed to
   accept_tcp to create an I/O port pair for each connection. */
_ ex_tcp_bind(ex *ex, _ host, _ port) {
    char *hostname  = CAST(cstring, host);
    int port_number = CAST_INTEGER(port);
    int fd;
    if (-1 == (fd = fd_socket(&ex->l, hostname, port_number, PORT_SOCKET_SERVER))) {
        ERROR("invalid", CONS(host, CONS(port, NIL)));
    }
    return integer_to_object(fd);
}

_ ex_unix_bind(ex *ex, _ node, _ force_delete) {
    char *nodename = CAST(cstring, node);
    int fd;
    if (force_delete != FALSE) {
        if (0 == remove(nodename)) {
            // _ex_printf(EX, "removed socket %s\n", nodename);
        }
    }
    if (-1 == (fd = fd_socket(&ex->l,
                              nodename, 0, 
                              PORT_SOCKET_UNIX | 
                              PORT_SOCKET_SERVER))) {
        ERROR("invalid", node);
    }
    return integer_to_object(fd);
}

_ ex_socket_accept(ex *ex, _ ob) {
    int server_fd = CAST_INTEGER(ob);
    int connection_fd = fd_accept(&ex->l, server_fd);
    if (-1 == connection_fd) ERROR("invalid-fd", ob);
    return CONS(_ex_fd_open(ex, connection_fd, "r", "I:tcp-accept"),
                _ex_fd_open(ex, connection_fd, "w", "O:tcp-accept"));
}

/* OS */
_ ex_system(ex *ex, _ ob) {
    char *cmd = object_to_cstring(ob);
    int rv = system(cmd);
    return integer_to_object(rv);
}



/* Processes */

#define STRING_ARGS(args, argc, argv)           \
    vector *v = CAST(vector, args);             \
    int argc = vector_size(v);                  \
    char *argv[argc+1];                         \
    int i;                                      \
    for (i=0; i<argc; i++) {                    \
        argv[i] = CAST(cstring, v->slot[i]);    \
    }                                           \
    argv[argc] = NULL;


_ _ex_open_process(ex *ex, _ args, char *cmode, int child_fd) {
    STRING_ARGS(args, argc, argv);
    int fd = fd_pipe(argv, NULL, child_fd);
    return _ex_leaf_to_object(ex, port_file_new(fdopen(fd, cmode), argv[0]));
}

_ ex_open_output_process(ex *ex, _ args) { return _ex_open_process(ex, args, "w", 0); }
_ ex_open_input_process(ex *ex, _ args) { return _ex_open_process(ex, args, "r", 1); }



/* This is one of the examples where it's simpler to code only a
   highlevel wrapper around select(), because of the data structures
   used (FD sets). */
#include <sys/select.h>
typedef struct {
    vector v;
    _ port;
    _ direction;
    _ ready;
} action;
#define PORTS_SET 1
#define PORTS_GET 2
static int for_actions(ex *ex, _ actions, fd_set *sets, int cmd) {
    int max = 0;
    _ a;
    for (a = actions; a != NIL; a = CDR(a)) {
        action *v = (action*)CAST(vector, CAR(a));
        if (vector_size((vector*)v) < 3) return ERROR("length", CAR(a));
        int dir = CAST_INTEGER(v->direction);
        if ((dir < 0) || (dir > 2)) return ERROR("direction", CAR(a));
        int fd;
        port *p;
        if ((p = object_to_port(v->port))) { fd = port_fd(p); }
        else fd = CAST_INTEGER(v->port);
        if (fd < 0) return ERROR("filedes", CAR(a));
        if (fd > max) max = fd;
        switch(cmd) {
        case PORTS_SET: FD_SET(fd, sets+dir); break;
        case PORTS_GET: v->ready = FD_ISSET(fd, sets+dir) ? TRUE : FALSE; break;
        }
    }
    return max;
}
_ ex_bang_select(ex *ex, _ actions, _ timeout) {
    fd_set sets[3];
    FD_ZERO(sets+0);
    FD_ZERO(sets+1);
    FD_ZERO(sets+2);
    int max = for_actions(ex, actions, sets, PORTS_SET);

    double seconds;
    inexact *isec = object_to_inexact(timeout);
    seconds = isec ? isec->value : object_to_integer(timeout);
    struct timeval tv = {(int)seconds,
                         (int)((seconds - (int)seconds) * 1000000.)};

    pthread_mutex_unlock(&ex->machine_lock);
    int rv = select(max + 1, sets+0, sets+1, sets+2, 
                    (timeout == FALSE) ? NULL : &tv );
    pthread_mutex_lock(&ex->machine_lock);

    if (-1 == rv) return ERROR("select", actions);
    for_actions(ex, actions, sets, PORTS_GET);
    return integer_to_object(rv);
}


/* Channels */

static leaf_object *read_chan_test(port *p) {
    return (leaf_object*)bytes_from_cstring("test");
}
static int write_chan_test(port *p, leaf_object *b) {
    if (leaf_type((leaf_object*)b) == bytes_type()) {
        fprintf(stderr, "GOT: %s\n", ((bytes*)b)->bytes);
    }
    leaf_free(b);
    return 0;
}
_ ex_make_channel_test_get(ex *ex) {
    channel *c = channel_new();
    channel_connect_producer(c, (channel_producer)read_chan_test, NULL);
    return _ex_leaf_to_object(ex, c);
}
_ ex_make_channel_test_put(ex *ex) {
    channel *c = channel_new();
    channel_connect_consumer(c, (channel_consumer)write_chan_test, NULL);
    return _ex_leaf_to_object(ex, c);
}


_ ex_channel_get(ex *ex, _ chan) {
    channel *c = object_to_channel(chan);
    /* Since closed channels change identity, we're robust here so
       multiple reads from the same object keep producing
       EOF_OBJECT. */
    if (!c) return EOF_OBJECT;

    leaf_object *l = channel_get(c);
    if (!l) {
        /* We close the channel because we get only one EOF
           notification.  The next read will deadlock. */
        ex_channel_close(ex, chan);
        return EOF_OBJECT;
    }
    else return _ex_leaf_to_object(ex, l);
}

/* This is intended as a barrier between EX and plain C code that
   operates without GC, and thus only supports leaf objects.  */
_ ex_channel_put(ex *ex, _ chan, _ ob) {
    leaf_object *l = ex->object_to_leaf(ex, ob);
    if (!l) TYPE_ERROR(ob);
    channel_put(CAST(channel, chan), l);
    ex->object_erase_leaf(ex, ob); // we no longer own it, so purge all references
    return VOID;
}


/* Open a process, and collect its output in chunks. */
static int write_chan_bytes(port *p, leaf_object *l) {
    if (l) { fprintf(stderr, "NULL!\n"); return 0; }
    leaf_type(l)->dump(l, p);
    port_flush(p);
    leaf_free(l);
    return 0;
}
_ ex_open_process_channels(ex *ex, _ args) {
    STRING_ARGS(args, argc, argv);
    int fd_i, fd_o;
    if (fd_pipe_2(argv, NULL, &fd_i, &fd_o)) return ERROR("invalid", args);
    // fprintf(stderr, "FD %d %d\n", fd_i, fd_o);
    port *p_i, *p_o;
    if (!(p_i = port_file_new(fdopen(fd_i, "w"), "<process-in>")))  return ERROR("bad-input-fd", integer_to_object(fd_i));
    if (!(p_o = port_file_new(fdopen(fd_o, "r"), "<process-out>"))) return ERROR("bad-output-fd", integer_to_object(fd_o));

    channel *c_i = channel_new();  
    channel *c_o = channel_new();  
    channel_connect_consumer(c_i, (channel_consumer)write_chan_bytes, (leaf_object*)p_i);
    channel_connect_producer(c_o, (channel_producer)port_slurp, (leaf_object*)p_o);

    return CONS(_ex_leaf_to_object(ex, c_i),
                _ex_leaf_to_object(ex, c_o));
}

/* Channels are shared objects.  */


_ ex_channel_close(ex *ex, _ chan) {
    leaf_object *l = (leaf_object*)CAST(channel, chan);
    leaf_free(l);
    ex->object_erase_leaf(ex, chan);
    return VOID;
}



/* Invoke a C continuation 
   
   This is a data barrier: C tasks can never have access to Scheme
   objects.  All data passes through a converter in the ck_manager.
*/


static leaf_object *test_ck(ck_class *m, leaf_object *obj) {
    printf("1: test_ck()\n"); obj = ck_yield(m, obj);
    printf("2: test_ck()\n"); obj = ck_yield(m, obj);
    printf("3: test_ck()\n"); obj = ck_yield(m, obj);
    return obj;
}


_ ex_with_ck(ex *ex, _ in_ref, _ value) {
    ck *task = NULL;
    ck_start fn = NULL;
    if (!(task = object_to_ck(in_ref))) {
        fn = (ck_start)test_ck;
    }
    ck *in_task = task;

    /* It's our (EX's) responsability to convert the object to/from
       leaf type.  Note that tasks are not allowed to store EX vector
       references nor leaf objects during suspend. */
    leaf_object *obj = ex->object_to_leaf(ex, value);
    if (!obj) return TYPE_ERROR(value);
    obj = LEAF_DUP(obj);

    ck_invoke(fn, &task, (void**)&obj);

    value = ex->leaf_to_object(ex, obj);

    /* Return value + context. */
    if (!task) return value; // task end
    if (in_task == task) { return CONS(value, in_ref); } // reuse task
    _ rv = CONS(value, ex->leaf_to_object(ex, (leaf_object*)task)); // create new task
    return rv;
}



/* Dynamic libraries */

#include <dlfcn.h>
_ ex_dlerror(ex *ex) {
    _ex_printf(ex, dlerror());
    _ex_printf(ex, "\n");
    return FALSE;
}
_ ex_dlopen(ex *ex, _ filename) {
    void *handle = dlopen(CAST(cstring, filename), RTLD_NOW);
    if (!handle) return ex_dlerror(ex);
    return const_to_object(GC_CHECK_ALIGNED(handle));
}
_ ex_dlclose(ex *ex, _ so) {
    dlclose(object_to_const(so));
    return VOID;
}
_ ex_dlsym(ex *ex, _ so, _ name) {
    void *addr = dlsym(object_to_const(so), CAST(cstring, name));
    if (!addr) return ex_dlerror(ex);
    return const_to_object(GC_CHECK_ALIGNED(addr));
}


_ ex_open_mode_tempfile(ex *ex, _ template, _ mode) {
    const char *cmode = CAST(cstring, mode);
    char *name = CAST(cstring, template);
    int fd = mkstemp(name);
    FILE *f = fdopen(fd, cmode);
    if (!f) { ERROR("invalid", CONS(template, CONS(mode, NIL))); }
    _ rv = _ex_make_file_port(ex, f, name);
    return rv;
}
_ ex_open_mode_file(ex *ex, _ path, _ mode) {
    bytes *b_path = CAST(bytes, path);
    bytes *b_mode = CAST(bytes, mode);
    FILE *f = fopen(b_path->bytes, b_mode->bytes);
    if (!f) ERROR("fopen", path);
    return _ex_make_file_port(ex, f, b_path->bytes);
}
