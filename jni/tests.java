package zwizwa.libprim;
public class tests {

    // (java invoke (java lookup (java type "dummy") "foo" (vector)) (void) (vector))
    public static void foo () {
        System.out.println("static foo() // method called");
    }
    public String state () {
        System.out.println("state() -> " + state + " // object method called");
        return state;
    }
    public static int addint (int a, int b) { return a + b; }
    public static tests make () { 
        System.out.println("static make() // method called");
        return new tests(); 
    }
    public tests() {
        state = "default";
        System.out.println("tests() // constructor called");
    }
    private String state;
    public tests(String s) {
        state = s;
        System.out.println("tests(" + s + ") // constructor called");
    }
}
