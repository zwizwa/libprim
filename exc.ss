#lang scheme/base

(require "tools.ss"
         "pp.ss")

;; Scheme -> EX compiler.  Essentially just `define' and `let*'.
;; C code emitter.

(define each for-each)
(define (each* fn . lsts)
  (apply each (lambda (x) (apply fn x)) lsts))



;; We're using just variable declarations and return statements.  The
;; rest are expressions.



(define (statement . _) (void))
(define (emit . _) (void))

(define (st-declaration var expr) (statement "_ ~a = ~a" var (expression expr)))
(define (st-expression expr)      (statement "~a" (expression expr)))
(define (st-return expr)          (statement "return ~a" (expression expr)))

(define (definition name formals)
  (emit "_ ~a(~a)"
        (map-name name)
        (arglist
         (map-def
          (map (add-type "_")
               (map symbol->string formals))))))

;(define (st-if cond yes-thunk no-thunk)
;  (with-block (format "if (FALSE != ~a) " (expression cond)) yes-thunk)
;  (with-block (format "else ") no-thunk))

(define (with-begin thunk)
  (emit "({")
  (with-indentation thunk)
  (emit "})"))



(define (expression x)
  (if (list? x)
      (format "~a(~a)"
              (map-name (car x))
              (arglist
               (map-app
                (map expression (cdr x)))))
      (format "~a" x)))


;; Direct compiler

(require mzlib/match)  ;; old matcher is more convenient

(define (ex-compile expr)
  (match
   expr
   (('define (name . formals) body)
    (begin
      (definition name formals)
      (emit "{")
      (with-indentation
       (lambda ()
         (emit "return")
         (ex-compile body)))
      (emit "}")))
   (('let* bindings body)
    (with-begin
     (lambda ()
       (for ((b bindings)) (apply st-declaration b))
       (ex-compile body))))
   (else
    (st-expression expr))))
              