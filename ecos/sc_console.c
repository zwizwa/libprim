#include <pkgconf/system.h>
#include <pkgconf/hal.h>
#include <string.h>

#include <cyg/infra/diag.h>
#include <cyg/kernel/kapi.h> 

#include <sc_vm1/vm1.h>

void _sc_ecos_init(cyg_addrword_t data) {
    char *script = (char*)data;
    char *argv[] = {"sc", "--bootstring", script};
    diag_printf("_sc_new:\n");
    sc *sc = _sc_new(3, (const char **)argv);
    diag_printf("_sc_continue:\n");
    _sc_continue(sc);
    diag_printf("_sc_ecos_init: EXIT\n");
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
