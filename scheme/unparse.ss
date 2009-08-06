#lang scheme/base

;; Convert s-expr -> C syntax.

(define (unparse x)
  (cond
   ((null? x) "NIL")
   ((pair? x)
    (format "CONS(~a,~a)"
            (unparse (car x))
            (unparse (cdr x))))
   ((number? x)
    (format "NUMBER(~s)" x))
   ((symbol? x)
    (format "SYMBOL(~s)" (symbol->string x)))
   (else "#<unprintable>")))

(define (evl x) (format "EVAL(SYNTAX(~a));\n" (unparse x)))

(define (convert)
  (let ((expr (read)))
    (unless (eof-object? expr)
      (display (evl expr))
      (convert))))

(convert)
