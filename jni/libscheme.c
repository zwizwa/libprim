/* Java JNI interface. */


/* Todo: separate LEAF and SC code */


#include <string.h>
#include <jni.h>


/* For test scripts we use a small embedded Scheme interpreter. */
#include <sc/scheme.h>
#include <leaf/tuple.h>


/* javah */
#include "libscheme.h_sc_prims"
static prim_def android_prims[] = libscheme_table_init;
void _libscheme_init(sc *sc) {
    _sc_def_prims(sc, android_prims);
}


#define LOGF(...) fprintf(stderr, __VA_ARGS__)

/* Dynamic context (VM running in the dynamic extent of a java call) */
typedef struct {
    JNIEnv *env;     // current Java thread's environment
    jclass cls;      // the embedding scheme class
    jmethodID call;  // C -> Java call delegation
    
    jclass type_string;
    jclass type_object;
    jclass type_object_array;
} java_ctx;
static inline java_ctx *_sc_java_ctx(sc *sc) {
    java_ctx *ctx = (java_ctx*)(EX->ctx);
    return ctx;
}
#define CTX _sc_java_ctx(sc)
#define JAVA_ENV (CTX->env)


typedef struct { leaf_class super; } jniref_class;

struct _jniref;
typedef struct _jniref jniref;

struct _jniref {
    leaf_object base;
    sc *sc;  // for JNIEnv*
    void *jni_ref;
};


/* The sc struct contains a reference to the currently active Java context. */
static void jniref_free(jniref *x) {
    // LOGF("DeleteRef(%p) RC=%d\n", x->jni_ref, leaf_rc((leaf_object*)x));
    sc *sc = x->sc;
    (*JAVA_ENV)->DeleteGlobalRef(JAVA_ENV, x->jni_ref);
    free(x);
}
static int jniref_write(jniref *x, port *p) {
    return port_printf(p, "#<jniref:%p>", x->jni_ref);
}
LEAF_SIMPLE_TYPE(jniref)
DEF_AREF_TYPE(jniref)

/* 1st level of wrapping: LEAF code, independent of EX/SC.

   FIXME: there is a dependency on the JNIEnv pointer, which is
   currently stored in a struct referenced by sc->ctx
 */
static jniref *jniref_new(void *jni_ref, sc *sc) {
    void *global_jni_ref =  (*JAVA_ENV)->NewGlobalRef(JAVA_ENV, jni_ref);
    // LOGF("local:%p, global:%p\n", jni_ref, global_jni_ref);
    (*JAVA_ENV)->DeleteLocalRef(JAVA_ENV, jni_ref);

    jniref *x = calloc(1, sizeof(*x));
    leaf_init(&x->base, jniref_type());
    x->sc = sc;
    x->jni_ref = global_jni_ref;
    // LOGF("jniref_free = %p\n", x->base.__type->_free);
    return x;
}
/* 2nd level wrapping */
static _ _sc_jniref(sc *sc, void *jni_ref) {
    if (!jni_ref) return VOID;
    else return _sc_make_aref(sc, jniref_new(jni_ref, sc));
}


/* Alternative: call a sc.java helper method. 

   This is the only place where java objects are created, so we get
   better control over the wrapping.  Essentially, delegate everything
   to the java side, with a "find" and "apply" style function
*/



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
    _libscheme_init(sc);
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

void METHOD(resume)(JNIEnv *env, jclass cls, jlong lsc) { 
    sc *sc = (void*)(long)lsc;
    java_ctx ctx;
    ctx.env = env;
    ctx.cls = cls;
    ctx.type_string = (*env)->FindClass(env, "java/lang/String");
    ctx.type_object = (*env)->FindClass(env, "java/lang/Object");
    ctx.type_object_array = (*env)->FindClass(env, "[java/lang/Object");

    LOGF("String:%p, Object:%p, Object[]:%p\n",
         ctx.type_string, ctx.type_object, ctx.type_object_array);

    ctx.call = 
        (*env)->GetStaticMethodID
        (env, cls, "_call", "([Ljava/lang/Object;)Ljava/lang/Object;");
                                         
    EX->ctx = &ctx;
    _sc_continue((void*)(long)lsc);
    EX->ctx = NULL;
}

/* Recursive EX -> Java data conversion. */
jarray _sc_vector_to_jarray(sc *sc, vector *v);
jobject _sc_object_to_jobject(sc *sc, _ ob) {
    const char *str;
    vector *vv;
    if ((str = object_to_cstring(ob))) 
        return (*CTX->env)->NewStringUTF(CTX->env, str);
    if ((vv = object_to_vector(ob))) 
        return (jobject)_sc_vector_to_jarray(sc, vv);
    else
        return (jobject)CAST(jniref, ob);
}
jarray _sc_vector_to_jarray(sc *sc, vector *v) {
    int i,n = vector_size(v);
    
    jarray a_j = (*CTX->env)->NewObjectArray(CTX->env, n, CTX->type_object, NULL);
    for (i=0; i<n; i++) {
        jobject o_j = _sc_object_to_jobject(sc, v->slot[i]);
        (*CTX->env)->SetObjectArrayElement(CTX->env, a_j, i, o_j);
        (*CTX->env)->DeleteLocalRef(CTX->env, o_j);
    }
    return a_j;
}

/* Recursive Java -> EX data conversion. */
_ _sc_jarray_to_object(sc *sc, jarray a_j);
_ _sc_jobject_to_object(sc *sc, jobject o_j) {
    jclass cls = (*CTX->env)->GetObjectClass(CTX->env, o_j);
    if (CTX->type_object_array == cls) \
        return _sc_jarray_to_object(sc, (jarray)o_j);
    if (CTX->type_string == cls) {
        _ ob;
        const char* str = (*CTX->env)->GetStringUTFChars(CTX->env, o_j, NULL);
        ob = SYMBOL(str);
        (*CTX->env)->GetStringUTFChars(CTX->env, o_j, NULL);
        (*CTX->env)->ReleaseStringUTFChars(CTX->env, o_j, str);
        return ob;
    }
    return _sc_jniref(sc, o_j);
}
_ _sc_jarray_to_object(sc *sc, jarray a_j) {
    int i,n = (*CTX->env)->GetArrayLength(CTX->env, a_j);
    vector *v = gc_alloc(EX->gc, n);
    for(i=0; i<n; i++) {
        v->slot[i] = _sc_jobject_to_object
            (sc, (*CTX->env)->GetObjectArrayElement(CTX->env, a_j, i));
    }
    return vector_to_object(v);
}



/* Tunnel everything through a single C -> Java call, and perform
   interpretation on the Java side. */
_ sc_java_call(sc* sc, _ cmd) {
    vector *v = CAST(vector, cmd);
    jarray a_j = _sc_vector_to_jarray(sc, v);
    jobject rv = (*CTX->env)->CallStaticObjectMethod
        (CTX->env, CTX->cls, CTX->call, a_j);
    // return _sc_jniref(sc, rv);
    return _sc_jobject_to_object(sc, rv);
}


_ sc_bang_def_toplevel(sc*, _, _);
void METHOD(setToplevel)(JNIEnv *env, jclass sc_class, jlong lsc,
                         jstring name, jobject value) {
    sc *sc = (void*)(long)lsc;
    const char *name_str = (*env)->GetStringUTFChars(env, name, NULL);
    _ name_sym = SYMBOL(name_str);
    (*env)->ReleaseStringUTFChars(env, name, name_str);
    fprintf(stderr, "setToplevel() needs global refs\n");
    sc_bang_def_toplevel(sc, name_sym, _sc_jniref(sc, value));
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



