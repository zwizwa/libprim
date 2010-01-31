/* Primitive exceptions. */

#ifndef _LEAF_ERROR_H_
#define _LEAF_ERROR_H_

#include <leaf/bytes.h>
#include <leaf/symbol.h>

/* Error abort. */
#include <setjmp.h>

/* This is called before unwinding the C stack, so data can point to a
   local struct. */
typedef void (*leaf_set_error)(leaf_ctx *ctx, int rv, void *data);

struct _leaf_ctx {
    jmp_buf buf;
    leaf_set_error set_error;
};

/* This needs to be a macro. */
#define leaf_catch(x) setjmp(((leaf_ctx*)x)->buf)

static inline void leaf_raise(leaf_ctx *ctx, int rv, void *data) {
    if (data) ctx->set_error(ctx, rv, data);
    longjmp(ctx->buf, rv);
}

#define EXCEPT_TRY     0
#define EXCEPT_LEAF    1 /* primitive leaf error */

#include <leaf/symbol.h>

typedef struct {
    symbol *sym;
    leaf_object *obj;
} leaf_error_info;


/* Raising primitive error messages. */

#define LEAF_ERROR_BUFSIZE 256
void leaf_raise_error(leaf_ctx *ctx, const char *tag, const char *fmt, ...);



#endif
