
import java.lang.reflect.*;

class DumpMethods {
    public static void main(String args[])
    {
        try {
            Class c = Class.forName(args[0]);
            Method m[] = c.getDeclaredMethods();
            for (int i = 0; i < m.length; i++)
                System.out.println(m[i].toString());
        }
        catch (Throwable e) {
            System.err.println(e);
        }
    }
}


public class testnative {

    void dump(String name)
    {
        try {
            Class c = Class.forName(name);
            Method m[] = c.getDeclaredMethods();
            for (int i = 0; i < m.length; i++)
                System.out.println(m[i].toString());
        }
        catch (Throwable e) {
            System.err.println(e);
        }
    }

    public static void main(String[] args) {
        testnative tn = new testnative();
        byte buf[]  = tn.loadFile ("testnative.h");

        // for(int i=0;i<buf.length;i++) System.out.print((char)buf[i]);
        tn.dump("java.lang.String");
        
    }
    native byte[] loadFile(String name);
    static {
        // without "lib" prefix and ".so" suffix
        System.loadLibrary("nativelib");
    }
}