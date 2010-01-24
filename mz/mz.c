#define _GNU_SOURCE
#include <dlfcn.h>
#include <link.h>
#include <stdio.h>

static int
callback(struct dl_phdr_info *info, size_t size, void *data) {
    int j;
    printf("name=%s (%d segments)\n", info->dlpi_name,
        info->dlpi_phnum);
    for (j = 0; j < info->dlpi_phnum; j++)
        printf("\t\t header %2d: type=%d, address=%10p\n", j, info->dlpi_phdr[j].p_type,
               (void *) (info->dlpi_addr + info->dlpi_phdr[j].p_vaddr));
    return 0;
}

int main(int argc, char **argv) {
    void *lib = dlopen(argv[1], RTLD_NOW);
    dl_iterate_phdr(callback, NULL);
    return 0;
}
