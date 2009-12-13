
/* A collection of static functions for Java reflection. 
   
   When in Rome...  The functionality in this file is intended for
   simplifying the libscheme.c JNI code.  A previous (failed) attempt
   proves that reflection and type casting are best done in Java, not
   in C using the JNI.
*/


package zwizwa.libprim;
import java.lang.reflect.*;

public class j {

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
    public static Object typeof (Object ... a) {
        return a[0].getClass();
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
    /* Convert Object[] array to Class[] array (Does java have
       recursive casts?)*/
    static private Class[] _types(Object o) {
        Object[] types = (Object[])o;
        Class[] ts = new Class[types.length];
        for (int i = 0; i<types.length; i++) ts[i] = (Class)types[i];
        return ts;
    }
    static Object lookup(Object... a) 
        throws java.lang.NoSuchMethodException
    {
        Class cls = (Class)a[0];
        String name = (String)a[1];
        Class[] ts = _types(a[2]);
        return cls.getDeclaredMethod(name, ts);
    }
    static Object constructor(Object... a)
        throws java.lang.NoSuchMethodException
    {
        Class cls = (Class)a[0];
        Class[] ts = _types(a[1]);
        return cls.getConstructor(ts);
    }
    static Object create(Object... a) 
        throws  java.lang.InstantiationException,
                java.lang.IllegalAccessException,
                java.lang.reflect.InvocationTargetException
    {
        Constructor ct = (Constructor)a[0];
        Object[] args = (Object[])a[1];
        return ct.newInstance(args);
    }
    static Object invoke(Object... a)
        throws java.lang.IllegalAccessException,
               java.lang.reflect.InvocationTargetException
    {
        Method m = (Method)a[1];
        Object[] args = (Object[])a[2];
        return m.invoke(a[0], args);
    }
    static Object string(Object... a) {
        Object o = a[0];
        Class c = o.getClass();
        if (c == String.class) return o;
        else if (c == Method.class) {
            String name = ((Method)o).getName();
            return "#<method:" + name + ">";
        }
        else if (c == Class.class) {
            String name = ((Class)o).getName();
            return "#<class:" + name + ">";
        }
        else if (c == Integer.class) {
            return ((Integer)o).toString();
        }
        else {
            String name = 
                o.getClass().getName() + ":" +
                Integer.toHexString(o.hashCode());
            return "#<object:" + name + ">";
        }
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
        else {
            System.out.print((String)string(o));
        }
    }
    static Object write(Object... a) { 
        _write(a[0]); 
        return null; 
    }
    static Object post(Object... a) { 
        System.out.print("-> "); 
        _write(a[0]); 
        System.out.print("\n"); 
        return null; 
    };


    /* The C bridge doesn't support numbers (yet), so tunnel them
       through strings. */
    static Object Long   (Object... a) { return new Long   ((String)a[0]); }
    static Object Integer(Object... a) { return new Integer((String)a[0]); }
    static Object Float  (Object... a) { return new Float  ((String)a[0]); }
    static Object Double (Object... a) { return new Double ((String)a[0]); }
    static Object Short  (Object... a) { return new Short  ((String)a[0]); }
    static Object Byte   (Object... a) { return new Byte   ((String)a[0]); }
}
