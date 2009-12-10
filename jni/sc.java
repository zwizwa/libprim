/* A collection of static functions to interact with a Scheme vm. */

public class sc {

    /** CLASS METHODS **/

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


    /* Constructor wraps C pointers in a Java object. */
    sc(long vm, long console) {
        _vm = vm;
        _console = console;
    }
    /* Factory */
    public static sc spawnConsole(String bootfile, String node) {
        long vm = boot(bootfile);
        long console = prepareConsoleServer(vm, node);
        spawnResume(vm);
        return new sc(vm, console);
    }

    /* Spawn vm in thread */
    public static Thread spawnResume(final long vm) {
        Thread t = new Thread() { public void run() { sc.resume(vm); }};
        t.start(); 
        return t;
    }


    /* Load the binary lib on application startup. */
    static {
        System.loadLibrary("sc");
    }


    /** INSTANCE METHODS **/

    long _vm = 0;
    long _console = 0;
    public String evalString(String commands) { 
        return consoleEvalString(_console, commands);
    }


}
