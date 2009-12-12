package zwizwa.libprim;
public class tests {

    static private void log (String str) {
        System.out.println("   *Java* " + str);
    }
    
    // (java invoke (java lookup (java type "dummy") "foo" (vector)) (void) (vector))
    public static void foo () {
        log("static foo() // method called");
    }
    public String state () {
        log("state() -> " + state + " // object method called");
        return state;
    }
    public static int addint (int a, int b) { return a + b; }
    public static tests make () { 
        log("static make() // method called");
        return new tests(); 
    }
    public tests() {
        state = "default";
        log("tests() // constructor called");
    }
    private String state;
    public tests(String s) {
        state = s;
        log("tests(" + s + ") // constructor called");
    }
}
