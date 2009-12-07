// package foo.bar;

/* Simple string/array representation of ADTs */

import java.lang.reflect.*;

public class adt {

    /* Globals & init */
    final Object ok = (Object)new Object[]{"ok"};
    final Object error = (Object)new Object[]{"error"};
    Class string_class;
    Class object_class;
    adt() {
        try {
            string_class = Class.forName("java.lang.String");
            object_class = Class.forName("java.lang.Object");
        }
        catch (Throwable e) {
            System.err.println(e);
        }
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
            System.out.print("#<object:" + o.getClass().getName() + ">");
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
        String[] s1 = new String[] {"a", "b", "c"};
        Object o1 = new Object[]{"post", new Object[]{"foo", "bar"}};
        Object o = (Object)s1;
        post(o);
        post("foo");
        post(o1);
        try { post(eval(o1)); }                     catch (Throwable e) {};
        //        try { post(safe_apply(find("eval"), o1)); } catch (Throwable e) {};
        try { post(find("eval")); }                 catch (Throwable e) {};
    }

    public static void main(String[] args) {
        adt x = new adt();
        x.go();
    }
}