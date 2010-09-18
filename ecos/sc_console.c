#include <pkgconf/system.h>
#include <pkgconf/hal.h>
#include <string.h>

#include <cyg/infra/diag.h>
#include <cyg/kernel/kapi.h> 

#include <sc_vm1/vm1.h>

void _sc_ecos_init(cyg_addrword_t data) {
    char *script = (char*)data;
    char *argv[] = {"sc", script};
    sc *sc = _sc_new(2, (const char **)argv);
    _sc_continue(sc);
}

cyg_uint8 app_stack[1024];
static cyg_thread app_thread;
static cyg_handle_t app_handle;

char script[] = "(display 123)";

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
