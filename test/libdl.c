#include <dlfcn.h>
int main (int argc, char **argv){
    void *module = dlopen("/dev/null", RTLD_NOW);
    return 0; // all well
}
