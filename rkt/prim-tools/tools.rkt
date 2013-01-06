#lang scheme/base

;; Tools used in the code generators.

(provide (all-defined-out))
(define (list->args lst)
  (if (null? lst) '()
      (cdr (apply append (for/list ((l lst)) (list ", " l))))))
(define (n->args n)
  (for/list ((i (in-range n)))
    (format "x~a" i)))
(define (string-append* lst) (apply string-append lst))
