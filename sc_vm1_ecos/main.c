#include "config.h"

#include <pkgconf/system.h>
#include <pkgconf/hal.h>
#include <string.h>

#include <cyg/infra/diag.h>
#include <cyg/kernel/kapi.h> 

#include <sc_vm1/vm1.h>

/* The boot.scm data embedded using objcopy .scm->.o converson. 

SYMBOL TABLE:
00000000 l    d  .rodata    00000000 .rodata
00000000 g       .rodata    00000000 _binary_boot_scm_start
00005de4 g       .rodata    00000000 _binary_boot_scm_end
00005de4 g       *ABS*	    00000000 _binary_boot_scm_size

The *ABS* symbol _binary_boot_scm_size symbol which holds the data
size is interpreted as the address of some variable, hence the `&'
operator below.

*/

extern const char _binary_boot_scm_start[];
extern const void _binary_boot_scm_size;
static const size_t _binary_boot_scm_nb_bytes = (size_t)&_binary_boot_scm_size;

void _sc_ecos_init(cyg_addrword_t data) {
    char bootsize[10];
    sprintf(bootsize, "%d", _binary_boot_scm_nb_bytes);
    const char *argv[] = {"sc", "--bootstring", _binary_boot_scm_start, "--bootsize", bootsize};
    sc *sc = _sc_new(5, (const char **)argv);
    _sc_continue(sc);
}

cyg_uint32 app_stack[10240];
static cyg_thread app_thread;
static cyg_handle_t app_handle;

char script[] = "(begin (display 123)(newline))";

void cyg_user_start(void)
{
    cyg_thread_create(0,
                      _sc_ecos_init,
                      (cyg_addrword_t)script,
                      "test thread",
                      app_stack,
                      sizeof(app_stack),
                      &app_handle,
                      &app_thread);

    /* After this function exits, the scheduler starts running resumed
       threads. */
    cyg_thread_resume(app_handle);
}
