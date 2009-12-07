// package foo.bar;

import java.lang.reflect.*;



public class reflect {

    String foo() { return "foo()"; }

    int test1(int a, int b) { return a + b; }
    Integer test2(Integer a, Integer b) { return a + b; }

    void call1 ()
    {
        try {
            Class int_class = Class.forName("java.lang.Integer");
            Class this_class = this.getClass(); //Class.forName("reflect");
            Class[] types = new Class[] {int_class, int_class};
            Method m = this_class.getDeclaredMethod("test1", types);
            Integer[] args = new Integer[] {1, 2};
            Object result = m.invoke(this, args);
            System.out.println((Integer)result);
        }
        catch (Throwable e) {
            System.err.println(e);
        }
    }

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
        reflect tn = new reflect();
        tn.dump(args[0]);
        // tn.call1();

        // tn.test1(1, 2);
        // Integer a = 1;
        // Integer b = 2;
        // tn.test2(a, b);
        // tn.test1(a, b);
    }
}