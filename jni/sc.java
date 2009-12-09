public class sc {
    /* Native methods */
    public native String eval(String commands);
    public native String startvm(String bootfile, String console, int detach);

    sc (String bootfile, String console, int detach) {
        startvm(bootfile, console, detach);
    }

    /* load the binary lib on application startup. */
    static {
        System.loadLibrary("sc");
    }

    public static void main(String[] arg) {
        sc x = new sc("../sc/boot.scm", "/tmp/sc", 0);
    }
}
