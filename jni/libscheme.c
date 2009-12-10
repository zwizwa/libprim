/* Java JNI interface. */

/*
  TODO: keep a reference table.  The problem is that we can't know
  whether an object gets wrapped twice, i.e. a method call might
  return an object that's already wrapped as a SC object.  The problem
  is then that we don't know when to "free" it.

  Essentially: we need a map from java object -> wrapped scheme object
  to prevent double wrapping.

*/


#include <string.h>
#include <jni.h>


/* For test scripts we use a small embedded Scheme interpreter. */
#include <sc/scheme.h>
#include <leaf/tuple.h>


/* javah */
#include "libsc.h_sc_prims"
static prim_def android_prims[] = libsc_table_init;
void _libsc_init(sc *sc) {
    _sc_def_prims(sc, android_prims);
}


#define LOGF(...) fprintf(stderr, __VA_ARGS__)

/* Scheme VM extensions. */
_ libsc_log(sc *sc,  _ str) {
    const char *msg = CAST(cstring, str);
    LOGF("%s", msg);
    return VOID;
}





/* JAVA JNI */

/* Except for memory management and some non-orthogonality of the API
   the JNI is quite straightforward.

   We're running the Scheme VM in the dynamic extent of a single JNI
   call which does not return under normal circumstances.

   The `this' argument passed and all references created by the JNI
   api are _local references_ which will be marked unused whenever the
   enclosing method finishes.

   Since that method exit never happens in our scenario we need to
   explicitly free any references that are passed in from the original
   JNI call or the results of any other calls in order not to leak
   memory or overflow the local reference table.

   Objects that need to survive the original JNI call that starts the
   interpreter need to be turned into global references.

   There seem to be 2 strategies:
     - turn _all_ references into global references
     - keep them local

   Currently: all are local, and the Scheme GC will trigger a
   DeleteLocalRef call.

   http://java.sun.com/j2se/1.4.2/docs/guide/jni/spec/functions.html
   http://journals.ecs.soton.ac.uk/java/tutorial/native1.1/implementing/refs.html

 */



/* Dynamic context (VM running in the dynamic extent of a java call) */
typedef struct {
    JNIEnv *env;      // current Java thread's environment
    jclass sc_class;  // the embedding scheme class
} java_ctx;
static inline java_ctx *_sc_java_ctx(sc *sc) {
    java_ctx *ctx = (java_ctx*)(EX->ctx);
    return ctx;
}
#define JAVA_ENV (_sc_java_ctx(sc)->env)


/* To support 1-1 wrapping (to avoid double wrapping which causes
   double release) we need to keep track of the Java objects. */
static tuple *java_pool = NULL;


typedef struct { leaf_class super; } jniref_class;
typedef struct {
    leaf_object base;
    sc *sc;  // for JNIEnv*
    void *jni_ref;
} jniref;

/* The sc struct contains a reference to the currently active Java context. */
static void jniref_free(jniref *x) {
    LOGF("DeleteLocalRef(%p) RC=%d\n", x->jni_ref, x->base._rc);
    sc *sc = x->sc;
    if (sc) {
        java_pool = tuple_list_remove_object(java_pool, (leaf_object*)x, 1);
        (*JAVA_ENV)->DeleteLocalRef(JAVA_ENV, x->jni_ref);
    }
    free(x);
}
static int jniref_write(jniref *x, port *p) {
    return port_printf(p, "#<jniref:%p>", x->jni_ref);
}
LEAF_SIMPLE_TYPE(jniref)
DEF_AREF_TYPE(jniref)

static int jobject_equal(jniref *x, void *jni_ref) { return x->jni_ref == jni_ref; }

/* 1st level wrapping makes a 1-1 map between leaf objects and jni references */
static jniref *new_jobject(sc *sc, void *jni_ref) {
    jniref *x;

    /* If it's already wrapped as jobject, return it with RC++.  */
    if ((x = (jniref*)tuple_list_find(java_pool, (leaf_predicate)jobject_equal, jni_ref))) {
        LOGF("reuse %p\n", jni_ref);
        return LEAF_DUP(x);
    }
    else {
        LOGF("wrap %p\n", jni_ref);
        x = calloc(1, sizeof(*x));
        leaf_init(&x->base, jniref_type());
        x->sc = sc;
        x->jni_ref = jni_ref;
        java_pool = tuple_stack_push(java_pool, (leaf_object*)x);        
        return x;
    }
}

/* 2nd level wrapping: this can't be avoided as these pointers are not
   stable due to GC moves, and there's no way to map jni_ref -> CS
   object. */
static _ _sc_jniref(sc *sc, void *jni_ref) {
    return _sc_make_aref(sc, new_jobject(sc, jni_ref));
}






/* Exceptions. 

   JNI methods give a special return value (i.e. NULL) to indicate an
   exception has been raised.  This needs to be handled _before_
   making another JNI call.

*/
_ _sc_java_check_error(sc *sc, _ err_ob, _ rv) {
    if ((*JAVA_ENV)->ExceptionCheck(JAVA_ENV)) {
        (*JAVA_ENV)->ExceptionDescribe(JAVA_ENV); // prints to stderr
        
        // jthrowable *e = (*JAVA_ENV)->ExceptionOccurred(JAVA_ENV);

        (*JAVA_ENV)->ExceptionClear(JAVA_ENV);
        return ERROR("java", err_ob);
    }
    else return rv;
}
_ sc_java_class(sc *sc, _ name) {
    const char *sname = CAST(cstring, name);
    jclass class = (*JAVA_ENV)->FindClass(JAVA_ENV, sname);
    if (!class) { return _sc_java_check_error(sc, name, VOID); }
    return _sc_jniref(sc, class);
}

/* For constructors use (java-methodID _class "<init>" "()V") */
_ sc_java_methodID(sc *sc, _ cls, _ name, _ sig) {
    const char *ssig = CAST(cstring, sig);
    jmethodID method = (*JAVA_ENV)->GetMethodID(JAVA_ENV, CAST(jniref, cls)->jni_ref,
                                                CAST(cstring, name), ssig);
    if (!method) return _sc_java_check_error(sc, name, VOID);
    return _sc_jniref(sc, method);
}
_ sc_java_static_methodID(sc *sc, _ cls, _ name, _ sig) {
    const char *ssig = CAST(cstring, sig);
    jmethodID method = (*JAVA_ENV)->GetStaticMethodID(JAVA_ENV, CAST(jniref, cls)->jni_ref,
                                                      CAST(cstring, name), ssig);
    if (!method) return _sc_java_check_error(sc, name, VOID);
    return _sc_jniref(sc, method);
}

/* FIXME: is this a local reference? */
_ sc_java_string(sc *sc, _ ob) {
    const char *str = CAST(cstring, ob);
    fprintf(stderr, "str: %s\n", str);
    return _sc_jniref(sc, (*JAVA_ENV)->NewStringUTF(JAVA_ENV, str));
}



/* Method calls in Java are a bit of a pain due to there not being a
   single root object.  The scalar objects need to be handled
   differently.  We use standard type description to convert Scheme
   objects to the proper jvalue type.  Note that object types
   themselves are not checked! */
static const char *sig_next(const char *c) {
    if (c[0] == 0) return NULL;
    if (c[0] == 'L') {
        for(;;) {
            if (c[0] == 0) return NULL;
            if (c[0] == ';') return c+1;
            c++;
        }
    }
    return c+1;
}
static int sig_nb_args(const char *c) {
    int i = 0;
    if (c[0] != '(') return -1;
    c++;
    for(;;) {
        printf("c = %p\n", c);
        if (c[0] == ')') return i;
        i++;
        c = sig_next(c);
        if (!c) return -1;
    }
}

/* For constructor (java-call _class _method #() "()V")  

   A full example, instantiating an object from the `eval' class using
   its default constructor:

   (let* ((cls (java-class "eval")) 
          (ctor (java-methodID cls "<init>" "()V")))
       (java-call cls ctor #() "()V"))
*/

static int _sc_jvalue_nargs(sc *sc, _ args) {
    return vector_size(CAST(vector, args));
}

static symbol *_sc_jvalue_args(sc *sc, _ args, _ signature, jvalue *jargs) {
    const char *argtype = CAST(cstring, signature);
    if (argtype[0] != '(') { ERROR("signature", signature); /* NR */ return NULL; }
    argtype++;

    vector *v = CAST(vector, args);
    int i = 0, n = vector_size(v);

    memset(jargs, 0, sizeof(jvalue)*n);

    for (i=0; i<n; i++) {
        _ so = v->slot[i];
        if (!argtype) { ERROR("nargs", args); /* NR */ return NULL; }
        switch(argtype[0]) {
        case 'Z': jargs[i].z = (so == FALSE) ? 0 : 1; break;
        case 'B': jargs[i].b = CAST_INTEGER(so); break;
        case 'C': jargs[i].c = CAST_INTEGER(so); break;
        case 'S': jargs[i].s = CAST_INTEGER(so); break;
        case 'I': jargs[i].i = CAST_INTEGER(so); break;
        case 'J': jargs[i].j = CAST_INTEGER(so); break;
        case 'F': jargs[i].f = object_to_double(so); break;
        case 'D': jargs[i].d = object_to_double(so); break;
        case 'L': jargs[i].l = CAST(jniref, so)->jni_ref; break;
        default:
            TYPE_ERROR(so);  /* NR */ return NULL;
        }
        argtype = sig_next(argtype);
    }
    if (argtype[0] != ')') { ERROR("signature", signature); /* NR */ return NULL; }
    argtype++;
    symbol *srtype = symbol_from_cstring(argtype);
    return srtype;
}

_ sc_java_new(sc *sc, _ jcls, _ ID, _ args, _ signature) {
    jvalue jargs[_sc_jvalue_nargs(sc, args)];
    symbol *srtype = _sc_jvalue_args(sc, args, signature, jargs);
    jniref  *cls = CAST(jniref, jcls);
    jniref *method = CAST(jniref, ID);
    jobject rv = (*JAVA_ENV)->NewObject(JAVA_ENV, cls->jni_ref, method->jni_ref, jargs);
    return _sc_java_check_error(sc, args, _sc_jniref(sc, rv));
}
_ sc_java_static_call(sc *sc, _ jcls, _ ID, _ args, _ signature) {
    jvalue jargs[_sc_jvalue_nargs(sc, args)];
    symbol *srtype = _sc_jvalue_args(sc, args, signature, jargs);
    jniref  *cls = CAST(jniref, jcls);
    jniref *method = CAST(jniref, ID);
    char rtype = srtype->name[0];
    jmethodID mid = method->jni_ref;
    jobject rv;
    switch(rtype) {
    case 'L': 
        rv = (*JAVA_ENV)->CallStaticObjectMethodA (JAVA_ENV, cls, mid, jargs); 
        return _sc_java_check_error(sc, args, _sc_jniref(sc, rv));
    case 'V': 
        (*JAVA_ENV)->CallStaticVoidMethodA (JAVA_ENV, cls, mid, jargs); 
        return _sc_java_check_error(sc, args, VOID); // check : no return value to signal error
    default:
        return ERROR("return-type", signature);
    }    
}
_ sc_java_call(sc *sc, _ ob, _ ID, _ args, _ signature) {
    jvalue jargs[_sc_jvalue_nargs(sc, args)];
    symbol *srtype = _sc_jvalue_args(sc, args, signature, jargs);

    jniref *method = CAST(jniref, ID);
    jniref *obj = CAST(jniref, ob);
    jobject rv;

    jobject o = obj->jni_ref;
    jmethodID mid = method->jni_ref;
    char rtype = srtype->name[0];
    switch(rtype) {
    case 'L': 
        rv = (*JAVA_ENV)->CallObjectMethodA (JAVA_ENV, o, mid, jargs); 
        return _sc_java_check_error(sc, args, _sc_jniref(sc, rv));
    case 'V': 
        (*JAVA_ENV)->CallVoidMethodA (JAVA_ENV, o, mid, jargs); 
        return _sc_java_check_error(sc, args, VOID); // check : no return value to signal error
    default:
        return ERROR("return-type", signature);
    }
}


/* Alternative: call a sc.java helper method. */






/* Library init */
jint JNI_OnLoad(JavaVM *vm, void *reserved) {
    return JNI_VERSION_1_2;
}


// see sc.h (generated from sc.class by javah)
#define METHOD(name) Java_zwizwa_libprim_sc_##name

/* Initialize VM. */
jlong METHOD(boot)(JNIEnv *env, jclass sc_class, jstring bootfile) {
    const char* bootfile_str = (*env)->GetStringUTFChars(env, bootfile, NULL);

    /* Boot from pre-expanded boot.scm */
    LOGF("Booting Scheme from %s\n", bootfile_str);
    char *argv[] = {"sc", "--boot", (char*)bootfile_str};  // FIXME: path (resource?)
    sc *sc = _sc_new(3, argv);
    _libsc_init(sc);
    (*env)->ReleaseStringUTFChars(env, bootfile, bootfile_str);
    LOGF("Scheme VM: %p\n", sc);

    /* If resume follows after this, we run the init script.  Override
       with different prepare for other behaviour. */
    _sc_prepare(sc, CONS(SYMBOL("eval"), CONS(SYMBOL("init-script"), NIL)));
    return (jlong)(long)sc;
}


jlong METHOD(prepareConsoleServer)(JNIEnv *env, jclass sc_class, jlong lsc, jstring usock) {
    sc *sc = (void*)(long)lsc;
    const char* usock_str = (*env)->GetStringUTFChars(env, usock, NULL);
    LOGF("Starting console on %s\n", usock_str);
    console *c = _sc_prepare_console_server(sc, usock_str);
    (*env)->ReleaseStringUTFChars(env, usock, usock_str);
    return (jlong)(long)c;
}

void METHOD(resume)(JNIEnv *env, jclass sc_class, jlong lsc) { 
    sc *sc = (void*)(long)lsc;
    java_ctx ctx = {env, sc_class};
    EX->ctx = &ctx;
    _sc_continue((void*)(long)lsc);
    EX->ctx = NULL;
}

/* Send a command to a console and collect the reply. */
jstring METHOD(consoleEvalString)(JNIEnv *env, jclass sc_class, jlong lconsole, jstring command) {
    console* sc_console = (console*)(long)lconsole;
    const char *command_str = (*env)->GetStringUTFChars(env, command, NULL);
    jstring rv = NULL;

    LOGF("RPC: %s\n", command_str);
    if (1) {
        tuple *reply = (tuple*)console_rpc(sc_console, command_str);

        /* Convert CONS-cell based s-expression AST wrapped in tuples
           to a vector/struct based ast wrapped in tuples.  This
           cannot represent Scheme vectors #(a b c) and improper lists
           (a b . c), only proper lists (a b c) */
        reply = tuple_ast_flatten_lin(reply);

        /* TODO: This gives symbol-tagged tuples that can be readily
           unpacked.  The first level wrapp is ok/error.  Based on the
           java side requirements mapping is done here.
         */

        bytes *b = leaf_to_string((leaf_object*)reply);
        rv = (*env)->NewStringUTF(env, b->bytes);
        leaf_free((leaf_object*)reply);
        leaf_free((leaf_object*)b);
    }
    else {
        bytes *b = console_rpc_bytes(sc_console, command_str);
        rv = (*env)->NewStringUTF(env, b->bytes);
        leaf_free((leaf_object*)b);
    }
    LOGF("RPC OK\n");
    (*env)->ReleaseStringUTFChars(env, command, command_str);
    return rv;
}


