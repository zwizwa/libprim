;; Bootstrap compiler for vm2 implemented in vm2 bytecode using vm1.

(load "compile.scm")

(define (read-map fn port)
  (let tail ()
    (let ((el (read port)))
      (if (eof-object? el) '()
          (let ((head (fn el)))
            (cons head (tail)))))))

(define *vm-macros* #f)

(define (compile-macros filename)
  (read-map (lambda (form)
              (display "compiling: ") (write (cadr form)) (newline)
              (cons (cadr form)
                    (eval (caddr form))))
            (open-input-file filename)))

(define (vm-macros)
  (if *vm-macros*
      *vm-macros*
      (begin
        (set! *vm-macros* (compile-macros "macros.scm"))
        *vm-macros*)))



;; Compiler macro dependencies.
;; (define vm-macros
;;   (list (cons 'quasiquote expand-quasiquote)
;;         (cons 'cond       expand-cond)
;;         (cons 'and        expand-and)
;;         (cons 'or         expand-or)
;;         (cons 'let        expand-let)
;;         (cons 'letrec     expand-letrec)
;;         (cons 'lambda     (lambda (e) ;; FIXME: old VM compat
;;                             (expand-lambda e (lambda (x) x))))))

;; Initial toplevel.
;; (define vm-toplevel-primitives
;;   (append
;;    (filter (lambda (x) (prim? (cdr x))) (toplevel))
;;    `((+             . ,add)
;;      (*             . ,mul))))

(define (vm-compile expr)
  (vm-compile/macros expr
                     '() ;; vm-toplevel
                     (vm-macros)))

(define (vm-run bytecode)
  (vm-init (vm-compile-anf bytecode))
  (vm-continue))

(define (vm-eval expr)
  (vm-run (car (vm-compile expr))))    ;; sexpr -> anf



(define (deps src)
  (let* ((out (vm-compile src))
         (code (car out))
         (deps (cadr out)))
    (filter (lambda (x) (not (prim? (eval x)))) deps)))

(define (vm1vm2)
  (deps (caddr (read (open-input-file "compile.scm")))))
