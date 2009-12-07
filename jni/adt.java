// package foo.bar;

/* Simple string/array representation of ADTs */

import java.lang.reflect.*;

public class adt {

    Class string_class = "".getClass();

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
    void post(Object o) {
        write(o);
        System.out.print("\n");
    }

    void go() {
        String[] s1 = new String[] {"a", "b", "c"};
        Object o1 = new Object[]{"a", "b", new Object[]{"e", "f"}};
        Object o = (Object)s1;
        post(o);
        post("foo");

    }

    public static void main(String[] args) {
        adt x = new adt();
        x.go();
    }
}