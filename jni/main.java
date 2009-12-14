
// package zwizwa.libprim;
import zwizwa.libprim.sc;

public class main {

    static void spawnConsoleServer() {
        sc x = sc.spawnConsoleServer("../sc/boot.scm", "/tmp/sc");
        String rv = x.evalString("(+ 1 2)");
        System.out.println("RV: " + rv);
        // x.evalString("(exit)");
        System.out.println("main() exit.");
    }

    static void startConsole() {
        sc.startConsole("../sc/boot-expanded.scm");
    }

    public static void main(String[] arg) {
        // spawnConsoleServer();
        startConsole();
    }
}