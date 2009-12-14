

/* A collection of static functions to interact with a Scheme vm. */

// Package names are necessary, as these are hard-coded in the C symbols.
package zwizwa.libprim;

import java.lang.reflect.*;
import zwizwa.libprim.j;

public class sc {


    /** CLASS METHODS **/

    /* Generic C->Java delegation method.  This gives access to the
       libprim.reflect class's static Object[] -> Object methods. */
    public static Object _call (Object... a) {
        Throwable erv;
        try {
            String cmd = (String)a[0];
            Object[] args = (Object[])a[1];
            Method m = j.class.getDeclaredMethod(cmd, new Class[] { Object[].class });
            Object rv = m.invoke(null, new Object[] { args });
            // return (Object) new Object[] {"ok", rv};
            return rv;
        }
        /* We don't propagate errors to C/Scheme (yet).  Just
           print them to the console. */
        catch (java.lang.reflect.InvocationTargetException e) {
            System.err.println(e.toString());
            System.err.println(e.getCause().toString());
            erv = e;
        }
        catch (Throwable e) {
            System.err.println(e.toString());
            erv = e;
        }
        // return (Object) new Object[] {"error", erv};
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
    public long _vm = 0;
    public long _console = 0;
    sc(long vm, long console) {
        _vm = vm;
        _console = console;
        setToplevel("this", this);
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
        return x;
    }

    /* Run blocking console on stdin. */
    public static void startConsole(String bootfile) {
        long vm = boot(bootfile);
        sc x = new sc(vm, 0);
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
}
