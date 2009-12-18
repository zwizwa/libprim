
/* A collection of static functions for Java reflection. 
   
   When in Rome...  The functionality in this file is intended for
   simplifying the libscheme.c JNI code.  A previous (failed) attempt
   proves that reflection and type casting are best done in Java, not
   in C using the JNI.
*/


package zwizwa.libprim;
import java.lang.reflect.*;
import java.io.BufferedReader;
import java.io.FileReader;

// Serialization through JSON format
// Get it here: http://www.json.org/java/json.zip
// import org.json.*;

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
        if (name.equals("int"))    return Integer.TYPE;
        if (name.equals("long"))   return Long.TYPE;
        if (name.equals("float"))  return Float.TYPE;
        if (name.equals("double")) return Double.TYPE;
        if (name.equals("void"))   return Void.TYPE;
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
        if (cls.isArray()) {
            Object[] in = (Object[])a[0];
            Object[] out = new Object[in.length]; 
            for (int i = 0; i < in.length; i++) { 
                out[i] = unpack(in[i]); 
            }
            return out;
        }
//         else if (cls == JSONArray.class) {
//             JSONArray in = (JSONArray)a[0];
//             Object[] out = new Object[in.length()]; 
//             for (int i = 0; i < in.length(); i++) { 
//                 try { out[i] = unpack(in.get(i)); }
//                 catch (JSONException e) {}
//             }
//             return out;
//         }
        else return a[0];
    }
    public static Object methods(Object ... a) {
        Class cls = (Class)a[0];
        return cls.getDeclaredMethods();
    }
    public static Object infoFields(Object... a) {
        for (Field f : ((Class)a[0]).getDeclaredFields()) {
            System.out.println(f.getName());
        }
        return null;
    }
    public static Object infoMethods(Object... a) {
        for (Method m : ((Class)a[0]).getDeclaredMethods()) {
            System.out.println(m.getName());
            for (Class c : m.getParameterTypes()) {
                System.out.println("   " + c.getName());
            }
            System.out.println("-> " + m.getReturnType().getName());
            System.out.println("");
        }
        return null;
    }
    public static Object info(Object... a) {
        infoFields(a[0]);
        infoMethods(a[0]);
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
    static Object method(Object... a) 
        throws java.lang.NoSuchMethodException
    {
        Class cls = (Class)a[0];
        String name = (String)a[1];
        Class[] ts = _types(a[2]);
        return cls.getDeclaredMethod(name, ts);
    }
    static Object field(Object... a) 
        throws java.lang.NoSuchFieldException
    {
        Class cls = (Class)a[0];
        String name = (String)a[1];
        return cls.getDeclaredField(name);
    }
    static Object get(Object... a) 
        throws java.lang.IllegalAccessException
    {
        Field fld = (Field)a[0];
        return fld.get(a[1]);
    }
    static Object getmember(Object... a) 
        throws java.lang.IllegalAccessException,
               java.lang.NoSuchFieldException
    {
        Class cls = a[0].getClass();
        return cls.getDeclaredField((String)a[1]).get(a[0]);
    }
    static Object set(Object... a) 
        throws java.lang.IllegalAccessException
    {
        Field fld = (Field)a[0];
        fld.set(a[1], a[2]);
        return null;
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
        else if (Number.class.isAssignableFrom(c)) {
            return ((Number)o).toString();
        }
        else if (c == Boolean.class) {
            return ((Boolean)o).toString();
        }
        else {
            return "#<object:" + o.toString() + ">";
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
    static Object Boolean(Object... a) { return new Boolean((String)a[0]); }

    static Object id (Object... a) { return a[0];}


//     static Object json (Object ... a) 
//         throws org.json.JSONException
//     {
//         String in = (String)a[0];
//         Object out = new JSONTokener(in).nextValue();
//         return unpack(out);  // JSONArray -> Object[]
//     }

    /* Read a file into a string. */
    static Object readfile (Object... a) 
        throws java.io.IOException 
    {
        String filename = (String)a[0];
        FileReader fr = new FileReader(filename);
        BufferedReader in = new BufferedReader(fr);
        String str;
        String out = "";
        while ((str = in.readLine()) != null) {
            out = out + " " + str;
        }
        in.close();
        return out;
    }

}
