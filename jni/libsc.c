
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


// see sc.h (generated from sc.class by javah)
#define METHOD(name) Java_sc_##name


/* Globals */
static console *sc_console = NULL;
pthread_mutex_t sc_mutex;  // serialize access to console

// #define LOCK()   pthread_mutex_lock(&sc_mutex)
// #define UNLOCK() pthread_mutex_unlock(&sc_mutex)

#define LOCK()
#define UNLOCK()

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
    JNIEnv *env;   // current Java thread's environment
    jobject this;  // the embedding scheme object
} java_ctx;
static inline java_ctx *_sc_java_ctx(sc *sc) {
    java_ctx *ctx = (java_ctx*)(EX->ctx);
    return ctx;
}
#define JAVA_ENV (_sc_java_ctx(sc)->env)


/* The sc struct contains a reference to the currently active Java context. */
static void _sc_java_free(sc *sc, jobject obj) {
    // LOGF("DeleteLocalRef(%p)", obj);
    // if (sc) (*JAVA_ENV)->DeleteLocalRef(JAVA_ENV, obj);
}

/* This macro defines standard LEAF object behaviour for wrapped Java
   JNI objects, and unwrappers for AREF representation. */

#define DEF_JAVA(typename) \
    typedef struct { leaf_class super; } java_##typename##_class; \
    typedef struct { leaf_object base; sc *sc; typename jni_ref; symbol *desc;} java_##typename; \
    void java_##typename##_free(java_##typename *x) { \
        _sc_java_free(x->sc, x->jni_ref); \
        free(x); \
    } \
    int java_##typename##_write(java_##typename *x, port *p) { \
        return port_printf(p, "#<" #typename ":%p:%s>", x->jni_ref, x->desc->name);       \
    } \
    LEAF_SIMPLE_TYPE(java_##typename) \
    java_##typename *java_##typename##_new(typename jni_ref, sc* sc, symbol *desc) {     \
        java_##typename *x = calloc(1, sizeof(*x)); \
        leaf_init((leaf_object*)x, java_##typename##_type());   \
        x->sc = sc; \
        x->desc = desc; \
        x->jni_ref = jni_ref; \
        return x; \
    }\
    DEF_AREF_TYPE(java_##typename)

DEF_JAVA(jclass)
DEF_JAVA(jobject)
DEF_JAVA(jmethodID)

/* Exceptions. 

   JNI methods give a special return value (i.e. NULL) to indicate an
   exception has been raised.  This needs to be handled _before_
   making another JNI call.

*/
_ _sc_java_check_error(sc *sc, _ err_ob, _ rv) {
    if ((*JAVA_ENV)->ExceptionCheck(JAVA_ENV)) {
        (*JAVA_ENV)->ExceptionClear(JAVA_ENV);
        return ERROR("java", err_ob);
    }
    else return rv;
}
_ sc_java_class(sc *sc, _ name) {
    const char *sname = CAST(cstring, name);
    jclass class = (*JAVA_ENV)->FindClass(JAVA_ENV, sname);
    if (!class) { return _sc_java_check_error(sc, name, VOID); }
    return _ex_leaf_to_object(EX, java_jclass_new(class, sc, symbol_from_cstring(sname)));
}
_ sc_java_methodID(sc *sc, _ cls, _ name, _ sig) {
    const char *ssig = CAST(cstring, sig);
    jmethodID method = (*JAVA_ENV)->GetMethodID(JAVA_ENV, CAST(java_jclass, cls)->jni_ref,
                                                CAST(cstring, name), ssig);
    if (!method) return _sc_java_check_error(sc, name, VOID);
    return _ex_leaf_to_object(EX, java_jmethodID_new(method, sc, symbol_from_cstring(ssig)));
}
/* Create a wrapped reference to the this pointer.  Pass sc = NULL to
   make sure the JNI reference doesn't get freed when the Scheme
   wrapper is collected. */
_ sc_java_this(sc *sc) {
    return _ex_leaf_to_object(EX, java_jobject_new(_sc_java_ctx(sc)->this, NULL,
                                                   symbol_from_cstring("Lcom/sony/smmts/scheme;")));
}
/* FIXME: is this a local reference? */
_ sc_java_string(sc *sc, _ ob) {
    return _ex_leaf_to_object
        (EX, java_jobject_new((*JAVA_ENV)->NewStringUTF(JAVA_ENV, CAST(cstring, ob)), sc,
                              symbol_from_cstring("Ljava/lang/String;")));
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
_ sc_java_call(sc *sc, _ ob, _ ID, _ args, _ signature) {
    const char *argtype = CAST(cstring, signature);
    if (argtype[0] != '(') return ERROR("signature", signature);
    argtype++;

    vector *v = CAST(vector, args);
    int i = 0, n = vector_size(v);
    jvalue jargs[n];
    for (i=0; i<n; i++) {
        _ so = v->slot[i];
        if (!argtype) return ERROR("nargs", ID);
        switch(argtype[0]) {
        case 'Z': jargs[i].z = (so == FALSE) ? 0 : 1; break;
        case 'B': jargs[i].b = CAST_INTEGER(so); break;
        case 'C': jargs[i].c = CAST_INTEGER(so); break;
        case 'S': jargs[i].s = CAST_INTEGER(so); break;
        case 'I': jargs[i].i = CAST_INTEGER(so); break;
        case 'J': jargs[i].j = CAST_INTEGER(so); break;
        case 'F': jargs[i].f = object_to_double(so); break;
        case 'D': jargs[i].d = object_to_double(so); break;
        case 'L': jargs[i].l = CAST(java_jobject, so)->jni_ref; break;
        default:
            return TYPE_ERROR(so);
        }
        argtype = sig_next(argtype);
    }
    if (argtype[0] != ')') return ERROR("signature", signature);
    argtype++;
    symbol *srtype = symbol_from_cstring(argtype);

    java_jmethodID *method = CAST(java_jmethodID, ID);
    java_jclass  *cls = object_to_java_jclass(ob);
    java_jobject *obj = object_to_java_jobject(ob);
    jobject rv;
    if (cls) {
        rv = (*JAVA_ENV)->NewObject(JAVA_ENV, cls->jni_ref, method->jni_ref, jargs);
        return _sc_java_check_error(sc, args, _ex_leaf_to_object(EX, java_jobject_new(rv, sc, srtype)));
    }
    else if (obj) {
        jobject o = obj->jni_ref;
        jmethodID ID = method->jni_ref;
        char rtype = argtype[0];
        switch(rtype) {
        case 'L': 
            rv = (*JAVA_ENV)->CallObjectMethodA (JAVA_ENV, o, ID, jargs); 
             return _sc_java_check_error(sc, args, _ex_leaf_to_object(EX, java_jobject_new(rv, sc, srtype)));
        case 'V': 
            (*JAVA_ENV)->CallVoidMethodA (JAVA_ENV, o, ID, jargs); 
            return _sc_java_check_error(sc, args, VOID); // check : no return value to signal error
        default:
            return ERROR("return-type", signature);
        }
    }
    else {
        return TYPE_ERROR(ob);
    }
}







/* Library init */
jint JNI_OnLoad(JavaVM *vm, void *reserved) {
    pthread_mutex_init(&sc_mutex, NULL); 
    return JNI_VERSION_1_2;
}


/* Initialize VM. */
jlong METHOD(boot)(JNIEnv *env,
                   jobject this,
                   jstring bootfile) {
    const char* bootfile_str = (*env)->GetStringUTFChars(env, bootfile, NULL);

    /* Boot from pre-expanded boot.scm */
    LOGF("Booting Scheme from %s\n", bootfile_str);
    char *argv[] = {"sc", "--boot", (char*)bootfile_str};  // FIXME: path (resource?)
    sc *sc = _sc_new(3, argv);
    _libsc_init(sc);
    (*env)->ReleaseStringUTFChars(env, bootfile, bootfile_str);
    LOGF("Scheme VM: %p\n", sc);
    return (jlong)(long)sc;
}


void METHOD(console)(JNIEnv *env,
                     jobject this,
                     jlong lsc,
                     jstring usock,
                     jint detach) {

    sc *sc = (void*)(long)lsc;

    const char* usock_str    = (*env)->GetStringUTFChars(env, usock, NULL);

    /* Start synchronous console. */
    java_ctx ctx = {env, this};
    EX->ctx = &ctx;

    LOGF("Starting console on %s\n", usock_str);

    _sc_start_console(sc, usock_str, &sc_console, detach);

    /* Kill reference to Java environment before returning to the Java
       caller.  This prevents any Scheme calls made otherwise to
       access the context.  To keep the Java context accessible, call
       the startvm() function in a Java thread and don't detach the VM
       mainloop, in which the previous function call does not
       return. */
    EX->ctx = NULL;
    (*env)->ReleaseStringUTFChars(env, usock, usock_str);

    LOGF("Scheme VM returned");
}



/* Evaluate Scheme string */
jstring METHOD(startvm)(JNIEnv *env, 
                        jobject this, 
                        jstring bootfile, 
                        jstring usock,
                        jint detach) {


    const char* bootfile_str = (*env)->GetStringUTFChars(env, bootfile, NULL);
    const char* usock_str    = (*env)->GetStringUTFChars(env, usock, NULL);


    if (sc_console) return (*env)->NewStringUTF(env, "VM already running");

    /* Boot from pre-expanded boot.scm */
    LOGF("Booting Scheme from %s\n", bootfile_str);
    char *argv[] = {"sc", "--boot", (char*)bootfile_str};  // FIXME: path (resource?)
    sc *sc = _sc_new(3, argv);
    _libsc_init(sc);

    (*env)->ReleaseStringUTFChars(env, bootfile, bootfile_str);

    LOGF("Scheme VM: %p\n", sc);
    
    /* Start synchronous console. */
    java_ctx ctx = {env, this};
    EX->ctx = &ctx;

    LOGF("Starting console on %s\n", usock_str);

    _sc_start_console(sc, usock_str, &sc_console, detach);

    /* Kill reference to Java environment before returning to the Java
       caller.  This prevents any Scheme calls made otherwise to
       access the context.  To keep the Java context accessible, call
       the startvm() function in a Java thread and don't detach the VM
       mainloop, in which the previous function call does not
       return. */
    EX->ctx = NULL;
    (*env)->ReleaseStringUTFChars(env, usock, usock_str);

    LOGF("Scheme VM returned");
    return (*env)->NewStringUTF(env, "VM returned");
}

jstring METHOD(eval)(JNIEnv *env, jobject this, jstring command) {
    const char *cmd_str;
    jstring rv = NULL;

    LOCK();
    cmd_str = (*env)->GetStringUTFChars(env, command, NULL);
    
    LOGF("RPC: %s\n", cmd_str);
    if (1) {
        tuple *reply = (tuple*)console_rpc(sc_console, cmd_str);

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
        bytes *b = console_rpc_bytes(sc_console, cmd_str);
        rv = (*env)->NewStringUTF(env, b->bytes);
        leaf_free((leaf_object*)b);
    }
    LOGF("RPC OK\n");
    (*env)->ReleaseStringUTFChars(env, command, cmd_str);

    UNLOCK();
    return rv;
}

