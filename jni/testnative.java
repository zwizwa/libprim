

public class testnative {
    native byte[] loadFile(String name);
    static {
        // without "lib" prefix and ".so" suffix
        System.loadLibrary("nativelib");
    }
}