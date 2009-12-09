/* A collection of static functions to interact with a Scheme vm. */

public class sc {
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

    public static void main(String[] arg) {
        long vm = boot("../sc/boot-expanded.scm");
        long console = prepareConsoleServer(vm, "/tmp/sc");

        if (false) {
            resume(vm);
        }
        else {
            spawnResume(vm);
            String rv = consoleEvalString(console, "(+ 1 2)");
            System.out.println("RV: " + rv);
            // consoleEvalString(console, "(exit)");
        }
        System.out.println("main() exit.");
    }
}
