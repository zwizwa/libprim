

public class testsc {
    public static void main(String[] arg) {
        sc x = new sc("../sc/boot.scm", "/tmp/sc");
        String rv = x.evalString("(+ 1 2)");
        System.out.println("RV: " + rv);
        // x.evalString("(exit)");
        System.out.println("main() exit.");
    }
}