(define vm-init void)
(define vm-continue void)

(define (undefined x) (error 'undefined (symbol->string x)))

(include "compile.scm")
