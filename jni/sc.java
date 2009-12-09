public class sc {
    /* Native methods */
    public native String eval(String commands);
    public native String startvm(int detach);

    /* load the binary lib on application startup. */
    static {
        System.loadLibrary("sc");
    }
}
