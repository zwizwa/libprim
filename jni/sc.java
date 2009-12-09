public class sc {
    /* Native methods */
    public native static String consoleEvalString(long console, String commands);

    /* Initialize the VM.  Returns a pointer cast as a long. */
    public native static long boot(String bootfile);

    /* */
    public native static void resume(long vm);

    /* Start a REPL server on a Unix console.  This offers two
       operation modes:
       
       detach = 0   

         Synchronous operation.  This runs the Scheme VM loop in the
         current execution contexts and so gives the Scheme VM access
         to the Java environment.  This is most useful if called from
         a Java thread.

       detach = 1

         Start the console in a native thread (pthread_create) and
         return from the function.  The Scheme VM has no access to the
         Java environment.
    */
    public native static long prepareConsoleServer(long vm, String node);


    /* Spawn vm in thread */
    public static Thread spawnResume(final long vm) {
        Thread t = new Thread() { public void run() { sc.resume(vm); }};
        t.start(); 
        return t;
    }


    /* load the binary lib on application startup. */
    static {
        System.loadLibrary("sc");
    }

    public static void main(String[] arg) {
        long vm = boot("../sc/boot-expanded.scm");
        long console = prepareConsoleServer(vm, "/tmp/sc");
        // resume(vm);
        spawnResume(vm);

        String rv = consoleEvalString(console, "(+ 1 2)");

        System.out.println("RV: " + rv);
    }
}
