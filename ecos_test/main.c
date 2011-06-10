
/* eCos test code. */

#include "config.h"

#include <pkgconf/system.h>
#include <pkgconf/hal.h>
#include <string.h>

#if 1 // eCos test
#include <cyg/infra/diag.h>
#include <cyg/kernel/kapi.h> 
#include <sc_vm1/vm1.h>
#include CYGHWR_MEMORY_LAYOUT_H
#endif


void app_exit(void) { for(;;); }

void test_io(const char *port) {
    FILE *io = fopen(port, "a+");
    diag_printf("%s : %x\n", port, (int)io);
    if (io) {
        const char msg[] = "Hello world!\n";
        int written = fwrite(msg, sizeof(msg), 1, io);
        diag_printf("write %s: %d, %d\n", port, written, ferror(io));
        fclose(io);
        cyg_thread_delay(1);
    }
}

void app_start(cyg_addrword_t data) {
#if 1
    test_io("/dev/ser0");
    test_io("/dev/ser1");
    test_io("/dev/ser2");
#endif
    test_io("/dev/haldiag");
    test_io("/dev/ttydiag");

    FILE *io = fopen("/dev/ttydiag", "a+");
    for(;;) {
        int c = fgetc(io);
        diag_printf("got %d\n", c);
        fputc(c, io);
    }
    app_exit();
}

cyg_uint32 app_stack[1024*2];
static cyg_thread app_thread;
static cyg_handle_t app_handle;

void cyg_user_start(void)
{
    diag_printf("libprim eCos test\n");
    cyg_thread_create(0, app_start, 0, "ecos_test",
                      app_stack, sizeof(app_stack),
                      &app_handle, &app_thread);

    /* After this function exits, the scheduler starts running resumed
       threads. */
    cyg_thread_resume(app_handle);
}
