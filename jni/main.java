
// package zwizwa.libprim;
import zwizwa.libprim.sc;

public class main {

    static void consoleTest(sc x) {
        String rv = x.evalString("(+ 1 2)");
        System.out.println("RV: " + rv);
        // x.evalString("(exit)");
        System.out.println("main() exit.");
    }

    static void startConsole(String[] args) {
        sc.startConsole(args);
    }

    public static void main(String[] args) {
        // consoleTest(sc.spawnConsoleServer("/tmp/sc", 0, args));
        consoleTest(sc.spawnConsoleServer("0.0.0.0", 12345, args));
        // startConsole(args);
    }
}