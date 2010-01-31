#include <leaf/error.h>

void leaf_raise_error(leaf_ctx *ctx, const char *tag, const char *fmt, ...) {
    leaf_error_info info = {
        symbol_from_cstring(tag),
        (leaf_object*)bytes_buffer_new(LEAF_ERROR_BUFSIZE)
    };
    va_list ap;
    va_start(ap, fmt);
    bytes_vprintf((bytes*)(info.obj), fmt, ap);
    va_end(ap);
    leaf_raise(ctx, EXCEPT_LEAF, &info);
}

