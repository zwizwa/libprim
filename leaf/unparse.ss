#lang scheme/base
(provide (all-defined-out))
;; Convert s-expr -> C syntax.

;;; C syntax

(define (unparse x)
  (cond
   ((null? x) "NIL")
   ((pair? x)
    (format "CONS(~a,~a)"
            (unparse (car x))
            (unparse (cdr x))))
   ((symbol? x)
    (format "SYMBOL(~s)" (symbol->string x)))
   ((string? x)
    (format "STRING(~s)" x))
   ((number? x)
    (format "NUMBER(~s)" x))
   (else "#<unprintable>")))

(define (evl x) (format "EVAL(~a);\n" (unparse x)))

(define (slurp)
  (let next ((lst '()))
    (let ((x (read)))
      (if (eof-object? x)
          (reverse lst)
          (next (cons x lst))))))

(define (convert)
  (define xs (slurp)) ;; make sure this succeeds before generating output
  (printf "static inline void _load(ex *ex){\n")
  (for ((x xs)) (display (evl x)))
  (printf "}\n"))


