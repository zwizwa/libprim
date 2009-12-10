/* A collection of static functions to interact with a Scheme vm. */

public class sc {

    /* Native methods.  Those that take C pointers cast as long are
       private for safety. */
    private native String consoleEvalString(long console, String commands);

    /* Initialize the VM.  Returns a pointer cast as a long. */
    private native long boot(String bootfile);

    /* Resume the VM in its current state (which can be set by a
       "prepare" method. */
    private native void resume(long vm);

    /* Prepare for starting a multi-head console server, and connect
       one pipe to a console object usable with consoleEvalString(). */
    private native long prepareConsoleServer(long vm, String node);


    private long vm = 0;
    private long console = 0;

    /* Public interface: use vm pointers stored in object. */
    public void resumeVM() { resume(vm); }

    /* Construct from symbolic data. */
    sc(String bootfile, String node) {
        vm = boot(bootfile);
        console = prepareConsoleServer(vm, node);
        spawnResume();
    }

    /* Spawn vm in thread */
    public Thread spawnResume() {
        Thread t = new Thread() { public void run() { resumeVM(); }};
        t.start(); 
        return t;
    }


    /* Load the binary lib on application startup. */
    static {
        System.loadLibrary("sc");
    }

    public String evalString(String commands) { 
        return consoleEvalString(console, commands);
    }


}
