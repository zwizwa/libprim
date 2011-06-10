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

/* The boot.scm data embedded using objcopy .scm->.o converson. 

SYMBOL TABLE:
00000000 l    d  .rodata    00000000 .rodata
00000000 g       .rodata    00000000 _binary_boot_scm_start
00005de4 g       .rodata    00000000 _binary_boot_scm_end
00005de4 g       *ABS*	    00000000 _binary_boot_scm_size

The *ABS* symbol _binary_boot_scm_size symbol which holds the data
size needs to be interpreted as the address of some variable, i.e. to
get to the size use:

   void _binary_boot_scm_size;
   int size = (int)(&_binary_boot_scm_size);

However, we just assume the data is properly zero-terminated which is
easier to work with in C.  Refer to project.mk for the .scm0 build
rule.

*/

extern const char _binary_boot_scm0_start[];

void _sc_ecos_init(cyg_addrword_t data) {
    const char *argv[] = {"sc", "--bootstring", _binary_boot_scm0_start};
    sc *sc = _sc_new(3, (const char **)argv);
    _sc_continue(sc);
}

cyg_uint32 app_stack[1024*2];
static cyg_thread app_thread;
static cyg_handle_t app_handle;

void cyg_user_start(void)
{
    diag_printf("starting scheme\n");
    cyg_thread_create(0, _sc_ecos_init, 0, "scheme",
                      app_stack, sizeof(app_stack),
                      &app_handle, &app_thread);

    /* After this function exits, the scheduler starts running resumed
       threads. */
    cyg_thread_resume(app_handle);
}
