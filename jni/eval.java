/* A tuple-based expression evaluation engine. */

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

    Method _find(String cmd) 
        throws java.lang.NoSuchMethodException {
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
    
    Object apply(Object... a) 
        throws java.lang.IllegalAccessException,
               java.lang.reflect.InvocationTargetException
    {
        Method m = (Method)a[0];
        return m.invoke(this, a[1]);
    }

    /* Design decisions:

       - strict vs. lazy (evaluate arguments before calling apply?)
       - pack/unpack argument arrays. */

    Object eval(Object... a)
        throws java.lang.NoSuchMethodException,
               java.lang.IllegalAccessException,
               java.lang.reflect.InvocationTargetException
    {
        Object[] e = (Object[])a[0];
        Class c = e[0].getClass();

        if (c == string_class) {
            String cmd = (String)e[0];
            if (cmd.equals("quote")) { return e[1]; }
            else {
                Method m = _find(cmd);
                return apply(m, e[1]);
            }
        }
        // else if (c == Method.class) {
        return apply((Method)e[0], e[1]);
        //}
    }


    void test() 
        throws java.lang.NoSuchMethodException,
               java.lang.IllegalAccessException,
               java.lang.reflect.InvocationTargetException
    {
        int i = 123;
        Object o = (Object)i;

        post(tuple("1", "2", "3"));
        post(eval(tuple("quote", tuple("abc", "123"))));
        post(apply(_find("write"), tuple(tuple("abc", "123"))));
        post(eval(tuple("write", tuple("123"))));
    }

    public static void main(String[] args) {
        eval x = new eval();
        try { x.test(); } catch (Throwable e) { System.err.println(e); }
    }
}