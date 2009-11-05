/* basic deps.
   anything that fails the stripped down libpf core, should fail here
*/

#define _GNU_SOURCE
#include <stdio.h>
#include <math.h>


int main (int argc, char **argv){

    char *string;
    if (-1 == asprintf(&string, "%d", 666)) return 1; // GNU asprintf

    // mix statements and var decls, code does this everywhere

    double one = cos(M_PI * 2); // libm

    return 0; // all well
}
