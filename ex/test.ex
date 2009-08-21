;; -*- scheme-mode -*-

;; I'm not sure yet if this is really useful.  The C syntax isn't too
;; bad, and there aren't so many primitives that need to be
;; implemented in C, can be expressed as "pure EX" and cannot be
;; expressed as Scheme code.

(define (foo a b)
  (let* ((x (bar a))
         (y (baz b)))
    (oof x y)))
