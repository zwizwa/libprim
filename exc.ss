#lang scheme/base

(require "tools.ss")
;; Scheme -> EX compiler.  Essentially just `define' and `let*'.

;; C code emitter.
(define emit-margin (make-parameter ""))
(define emit-indent
  (make-parameter
   (lambda (str)
     (string-append str "    "))))
(define p-emit
  (make-parameter
   (lambda (str)
     (display (emit-margin))
     (display str)
     (newline))))

(define (emit fmt . args)
  ((p-emit) (apply format fmt args)))

(define (with-indentation thunk)
  (parameterize
      ((emit-margin ((emit-indent) (emit-margin))))
    (thunk)))

(define each for-each)
(define (each* fn . lsts)
  (apply each (lambda (x) (apply fn x)) lsts))



;; We're using just variable declarations and return statements.  The
;; rest are expressions.

(define (statement fmt . args)   (apply emit (string-append fmt ";") args))
(define (s-declaration var expr) (statement "_ ~a = ~a" var (expression expr)))
(define (s-return expr)          (statement "return ~a" (expression expr)))

(define (add-type t) (lambda (x) (format "~a ~a" t x)))
(define (pointer t) (format "*~a" t))
(define (definition name formals)
  (emit "_ ~a(~a)"
        (map-name name)
        (arglist
         (map-def
          (map (add-type "_")
               (map symbol->string formals))))))

(define (with-block thunk)
  (emit "{")
  (with-indentation thunk)
  (emit "}"))

;; Expressions are composed recursively as strings.
;; Constructor/function names use a global translation (to enable
;; namespace prefix), and argument lists are translated (to allow for
;; data context prefix).

(define name-context (make-parameter "ex"))

(define ((if-ctx fn) x)
  (let ((ctx (name-context)))
    (if ctx (fn ctx x) x)))

(define map-name (if-ctx (lambda (ctx x) (format "~a_~a" ctx x))))
(define map-app  (if-ctx (lambda (ctx lst) (cons ctx lst))))
(define map-def  (if-ctx (lambda (ctx lst)
                           (cons ((add-type ctx)
                                  (pointer ctx)) lst))))

(define (arglist lst)
  (string-append* (list->args lst)))
(define (expression x)
  (if (list? x)
      (format "~a(~a)"
              (map-name (car x))
              (arglist
               (map-app
                (map expression (cdr x)))))
      (format "~a" x)))


;; MACROS
(define-syntax-rule
  (block ((var expr) ...) body-expr)
  (with-block
   (lambda ()
     (s-declaration 'var 'expr) ...
     body-expr)))
(define-syntax-rule (return x)
  (s-return 'x))

(define-syntax-rule (def (name . formals) body)
  (begin
    (definition 'name 'formals)
    body))

(def (bar x y)
  (block ((a 123)
          (b 345))
    (block ((c (plus a b))
            (d (min a)))
       (return (foo c d x y)))))