

/* A collection of static functions to interact with a Scheme vm. */

// Package names are necessary, as these are hard-coded in the C symbols.
package zwizwa.libprim;

import java.lang.reflect.*;

public class sc {


    /** CLASS METHODS **/

    /* Methods prefixed with "_" are all vararg Object methods that
       produce a single Object.  This encodes a run-time typed
       representation of structured data (with Object[] as basic
       structure type) which maps better to Scheme. */

    public static Object _class (Object ... a) 
        throws java.lang.ClassNotFoundException
    {
        String name = (String)a[0];
        /* Is there a textual representation of primitive types? */
        if (name.equals("int"))  return Integer.TYPE;
        if (name.equals("long")) return Long.TYPE;
        if (name.equals("void")) return Void.TYPE;
        /* A proper class: access through global namespace. */
        return Class.forName(name);
    }
    public static Object _tuple (Object ... a) {
        return (Object)a;
    }
    public static Object _test (Object ... a) {
        return _tuple("foo", "bar");
    }

    /* Generic C->Java delegation method. */
    public static Object _call (Object ... a) {
        try {
            Class c = a[0].getClass();
            /* Late-bound command. */
            if (c == String.class) {
                String cmd = (String)a[0];
                if (cmd.equals("class")) return _class(a[1]);
                if (cmd.equals("test")) return _test();
                return null;
            }
            /* Class method */
            else if (c == Class.class) {
            }
            /* Object method */
            else {
            }
        }
        catch (Throwable e) {
            /* We don't propagate errors to C (yet).  Just print them
               to the console. */
            System.err.println(e.toString());
        }
        return null;
    }


    /* Native methods */
    public native static String consoleEvalString(long console, String commands);

    /* Initialize the VM.  Returns a pointer cast as a long. */
    public native static long boot(String bootfile);

    /* Resume the VM in its current state (which can be set by a
       "prepare" method. */
    public native static void resume(long vm);

    /* Prepare for starting a multi-head console server, and connect
       one pipe to a console object usable with consoleEvalString(). */
    public native static long prepareConsoleServer(long vm, String node);


    /* Add name.value bindings to the toplevel env. */
    public native static void setToplevel(long vm, String name, Object value);


    /* Constructor wraps C pointers in a Java object. */
    long _vm = 0;
    long _console = 0;
    sc(long vm, long console) {
        _vm = vm;
        _console = console;
    }
    /* Factory methods */

    /* Create a VM instance wrapped in a java object.  Its main loop
       runs in a Java thread, and handles console connections on the
       Unix socket `node', and interprets commands passed in by the
       evalString method.  */
    public static sc spawnConsoleServer(String bootfile, String node) {
        long vm = boot(bootfile);
        long console = prepareConsoleServer(vm, node);
        spawnResume(vm);
        sc x = new sc(vm, console);
        x.setThis();
        return x;
    }

    /* Run blocking console on stdin. */
    public static void startConsole(String bootfile) {
        long vm = boot(bootfile);
        resume(vm);
    }

    /* Spawn vm in thread */
    public static Thread spawnResume(final long vm) {
        Thread t = new Thread() { public void run() { sc.resume(vm); }};
        t.start(); 
        return t;
    }


    /* Load the binary lib on application startup. */
    static {
        System.loadLibrary("scheme");
    }


    /** INSTANCE METHODS **/
    public String evalString(String commands) { 
        return consoleEvalString(_console, commands);
    }
    public void setToplevel(String name, Object value) {
        setToplevel(_vm, name, value);
    }
    public void setThis() {
        setToplevel("this", this);
    }
}
