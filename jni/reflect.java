
/* A collection of static functions for Java reflection. */

// Package names are necessary, as these are hard-coded in the C symbols.
package zwizwa.libprim;

import java.lang.reflect.*;

public class reflect {

    /* Vararg Object methods that produce a single Object.  This
       encodes a run-time typed representation of structured data
       (with Object[] as basic structure type) which maps better to
       Scheme. */

    public static Object type (Object ... a) 
        throws java.lang.ClassNotFoundException
    {
        String name = (String)a[0];
        /* Is there a textual representation of primitive types? */
        if (name.equals("int"))  return Integer.TYPE;
        if (name.equals("long")) return Long.TYPE;
        if (name.equals("void")) return Void.TYPE;
        /* A proper class: access through global namespace. */
        return Class.forName(name);
    }
    public static Object tuple (Object ... a) {
        return (Object)a;
    }
    // (java-call #("test" #()))
    // (java test)
    public static Object test (Object ... a) {
        return tuple("foo", "bar");
    }
    /* Unpack any kind of nested array into a nested Object array
       structure.  */
    public static Object unpack(Object ... a) {
        Class cls = a[0].getClass();
        if (!cls.isArray()) return cls;
        else {
            Object[] in = (Object[])a[0];
            Object[] out = new Object[in.length]; 
            for (int i = 0; i < in.length; i++) { 
                out[i] = unpack(in[i]); 
            }
            return out;
        }
    }
    public static Object methods(Object ... a) {
        Class cls = (Class)a[0];
        return cls.getDeclaredMethods();
    }
    public static Object info(Object... a) {
        Method m[] = ((Class)a[0]).getDeclaredMethods();
        for (int i = 0; i < m.length; i++) {
            // System.out.println(m[i].toString());
            System.out.println(m[i].getName());
            Class[] cs = m[i].getParameterTypes();
            for (int j = 0; j < cs.length; j++) {
                System.out.println("   " + cs[j].getName());
            }
            System.out.println("-> " + m[i].getReturnType().getName());
            System.out.println("");
        }
        return null;
    }
    private static void _write(Object o) {
        Class c = o.getClass();
        if (c.isArray()) {
            Object[] os = (Object[])o;
            System.out.print("(");
            for (int i = 0; i < os.length; i++) {
                if (i != 0) System.out.print(" ");
                _write(os[i]);
            }
            System.out.print(")");
        }
        else if (c == String.class) {
            System.out.print((String)o);
        }
        else {
            String name = 
                o.getClass().getName() + ":" +
                Integer.toHexString(o.hashCode());
            // String name = o.toString();
            System.out.print("#<object:" + name + ">");
        }
    }
    static Object write(Object... a) { _write(a[0]); return null; }
    static Object post(Object... a) { _write(a[0]); System.out.print("\n"); return null; };
}
