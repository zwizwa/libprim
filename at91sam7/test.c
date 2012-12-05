#include "os.h"
void echo(void) {
    while (1) {
        int c = dbgu_read();
        if (c == '\r') {
            dbgu_write('\n');
        }
        dbgu_write(c);
    }
}
int main(void) {
    echo();
    return 0;
}
