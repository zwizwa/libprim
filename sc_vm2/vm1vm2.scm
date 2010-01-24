;; Bootstrap compiler for vm2 implemented in vm2 bytecode using vm1.

(load "macros.scm")
(load "compile.scm")

(define (deps src)
  (let* ((out (vm-compile src))
         (code (car out))
         (deps (cadr out)))
    (filter (lambda (x) (not (prim? (eval x)))) deps)))

(define (vm1vm2)
  (deps (caddr (read (open-input-file "compile.scm")))))
