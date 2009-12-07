// package foo.bar;

/* Simple string/array representation of ADTs */

import java.lang.reflect.*;
import java.util.Hashtable;


class dummy {
    String x;
    dummy() {
        x = "foo";
    }
    dummy(String init) {
        x = init;
    }
}





public class adt {

    /* Globals & init */
    final Object ok = (Object)new Object[]{"ok"};
    final Object error = (Object)new Object[]{"error"};
    Class string_class;
    Class object_class;
    // Class object_array_class;
    final Hashtable store = new Hashtable();
    adt() {
        try {
            string_class = Class.forName("java.lang.String");
            object_class = Class.forName("java.lang.Object");
            // ????
            // object_array_class = (Array.newInstance(object_class, 0)).getClass();
        }
        catch (Throwable e) {
            System.err.println(e);
        }
    }
    Object arr(Object... a) { return (Object)a; }


    /* Store access */
    Object get (Object o) {
        Object[] arg = (Object[])o;
        return store.get(arg[0]);
    }
    Object put (Object o) {
        Object[] arg = (Object[])o;
        store.put(arg[0], arg[1]);
        return null;
    }


    


    /* Object Creation */
    Object create (Object o) 
    {
        try {
            Object[] arg = (Object[])o;
            String classname = (String)arg[0];
            Class cls = Class.forName(classname);
            return (Object)cls.newInstance();
        }
        catch (Throwable e) { return null; }
    }

    /* Prettyprinting */
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
    Object post(Object o) {
        _write(o);
        System.out.print("\n");
        return ok;
    }


    /* Reflection: string -> method lookup. */
    Method find(String cmd) 
        throws java.lang.NoSuchMethodException
    {
        return this.getClass().getDeclaredMethod(cmd, new Class[] {object_class});
    }
    Object eval(Object o) 
        throws java.lang.NoSuchMethodException,
               java.lang.IllegalAccessException,
               java.lang.reflect.InvocationTargetException
    {
        Object[] a = (Object[])o;
        String cmd = (String)a[0];
        Object args = (String)a[1];
        Method m = find(cmd);
        return m.invoke(this, args);
    }

    Object safe_apply(Method m, Object o) {
        try { 
            Object rv =  m.invoke(this, o);
            return (Object)new Object[]{"ok", rv};
        }
        catch (Throwable e) {
            System.err.println("ERROR: " + e.toString());
            return error;
        }
    }

    void go() {

        Object report =
        new Object[]{
            new Object[] {"video", new Object[] {"quality", "0.1"}},
            new Object[] {"audio", new Object[] {"quality", "1.2"}}
        };

        String[] s1 = new String[] {"a", "b", "c"};
        Object o1 = new Object[]{"post", new Object[]{"foo", "bar"}};
        Object o = (Object)s1;


            // post(o);
            // post("foo");
            // post(o1);
            // try { post(eval(o1)); }                     catch (Throwable e) {};
            // try { post(safe_apply(find("eval"), o1)); } catch (Throwable e) {};
            // try { post(find("eval")); }                 catch (Throwable e) {};
            // post(report);
            
            // put(arr("dummy", create(arr("dummy"))));
            // post(get(arr("dummy")));
            
            
            // put(arr("dummy", "foo"));
            // post(get(arr("dummy")));

        try { post(eval(arr("post", "123"))); } catch (Throwable e) {};

    }

    public static void main(String[] args) {
        adt x = new adt();
        x.go();
    }
}