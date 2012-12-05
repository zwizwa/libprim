#include <string.h>
#include <sys/stat.h>

#include "os.h"
int _write(int file, const char *ptr, int len) {
    if (file != 1) return -1; // Only stdout is supported.
    int i;
    for (i=0; i<len; i++) dbgu_write(ptr[i]);
    return len;
}

/* See .ld script: heap squeezed inbetween .bss and stack space. */
extern char _stack_end;
extern char _bss_end;
static char *heap_end = (&_bss_end);
#define heap_top (&_stack_end)

caddr_t _sbrk(int incr) {
    char *next_heap_end = heap_end + incr;

    /* Check for heap and stack collision.  Maybe it's best to leave
       this out and let the app manage stack/heap border? */
    if (next_heap_end > heap_top) {
        return (caddr_t)0;
    }
    else {
        heap_end = next_heap_end;
        return (caddr_t)heap_end;
    }
}
int _fstat(int file, struct stat *st) {
    st->st_mode = S_IFCHR;
    return 0;
}
int _isatty(int file) {
    return 1;
}

static void not_implemented(const char *func) {
    _write(1, "NOT_IMPLEMENTED: ", 17);
    _write(1, func, strlen(func));
    _write(1, "\r\n", 2);
}
#define NI not_implemented(__func__); return -1
int _read(void)   { NI; }
int _close(void)  { NI; }
int _lseek(void)  { NI; }
#undef NI
