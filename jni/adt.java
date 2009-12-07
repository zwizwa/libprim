// package foo.bar;

/* Simple string/array representation of ADTs */

import java.lang.reflect.*;

public class adt {


    Object ok = (Object)new Object[]{"ok"};
    Object error = (Object)new Object[]{"error"};

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

    void write(Object o) {
        if (o.getClass() == string_class) {
            System.out.print((String)o);
        }
        else {
            Object[] os = (Object[])o;
            System.out.print("(");
            for (int i = 0; i < os.length; i++) {
                if (i != 0) System.out.print(" ");
                write(os[i]);
            }
            System.out.print(")");
        }
    }
    Object safe_apply(Method m, Object o) {
        try { 
            Object rv =  m.invoke(this, o);
            return (Object)new Object[]{"ok", rv};
        }
        catch (Throwable e) {
            System.err.println("ERROR: " + e);
            return error;
        }
    }

    Object post(Object o) {
        try {
            write(o);
            System.out.print("\n");
            return ok;
        }
        catch (Throwable e) {
            System.err.println(e);
            return error;
        }
    }
    Object eval(Object o) {
        try {
            Object[] rec = (Object[])o;
            String cmd = (String)rec[0];
            Method m = this.getClass().getDeclaredMethod(cmd, new Class[] {object_class});
            return m.invoke(this, rec[1]);
        }
        catch (Throwable e) {
            System.err.println(e);
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
        eval(o1);
    }

    public static void main(String[] args) {
        adt x = new adt();
        x.go();
    }
}