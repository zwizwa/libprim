public class sc {
    /* Native methods */
    public native String eval(String commands);
    public native String startvm(String bootfile, String console, int detach);


    /* Initialize the VM.  Returns a pointer cast as a long. */
    public native static long boot(String bootfile);

    /* Start Unix console. */
    public native static void console(long vm, String node, int detach);

    sc (String bootfile, String console, int detach) {
        startvm(bootfile, console, detach);
    }

    /* load the binary lib on application startup. */
    static {
        System.loadLibrary("sc");
    }

    public static void main(String[] arg) {
        // sc x = new sc("../sc/boot-expanded.scm", "/tmp/sc", 0);

        long vm = boot("../sc/boot-expanded.scm");
        console(vm, "/tmp/sc", 0);
    }
}
