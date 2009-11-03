#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"

int main(int argc, char **argv) {
    sc *sc = _sc_new(NULL, argv[1]);
    return 0;
}
