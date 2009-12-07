
public class testnative {
    public static void main(String[] args) {
        testnative tn = new testnative();
        byte buf[]  = tn.loadFile ("testnative.h");

        for(int i=0;i<buf.length;i++) {
            System.out.print((char)buf[i]);
        }
        
    }
    native byte[] loadFile(String name);
    static {
        // without "lib" prefix and ".so" suffix
        System.loadLibrary("nativelib");
    }
}