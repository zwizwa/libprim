#define _GNU_SOURCE
#include <dlfcn.h>
#include <link.h>
#include <stdio.h>

static int
callback(struct dl_phdr_info *info, size_t size, void *data) {
    printf("%s\n", info->dlpi_name);
}

int main(int argc, char **argv) {
    void *lib = dlopen(argv[1], 0);
    dl_iterate_phdr(callback, NULL);
    return 0;
}
