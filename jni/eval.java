import java.lang.reflect.*;

public class eval {

    /* Cache & Init */
    final Object _void = (Object)new Object[]{"void"};
    final Object _error = (Object)new Object[]{"error"};
    Class string_class;
    Class object_class;
    Class object_array_class;
    eval () {
        try {
            string_class = Class.forName("java.lang.String");
            object_class = Class.forName("java.lang.Object");
            object_array_class = Object[].class;
        }
        catch (Throwable e) {
            System.err.println(e);
        }
    }

    /* Util.  Methods that are prefixed with '_' (underscore) do not
       respect the Object[] -> Object type. */

    Method _find(String cmd) throws java.lang.NoSuchMethodException {
        return this.getClass().getDeclaredMethod(cmd, new Class[] {object_array_class});
    }
    Object tuple(Object... a) { return (Object)a; }

    void _write(Object o) {
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
        else if (c == string_class) {
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

    Object write(Object... a) { _write(a[0]); return _void; }
    Object post(Object... a) { _write(a[0]); System.out.print("\n"); return _void; };
    
    void test() 
        throws java.lang.NoSuchMethodException,
               java.lang.IllegalAccessException,
               java.lang.reflect.InvocationTargetException
    {
        write(tuple("1", "2", "3"));
        // find("print_2").invoke(this, new Object[] {args});
    }

    public static void main(String[] args) {
        eval x = new eval();
        try { x.test(); } catch (Throwable e) { System.err.println(e); }
    }
}