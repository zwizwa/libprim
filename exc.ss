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

(define (with-block thunk)
  (emit "{")
  (with-indentation thunk)
  (emit "}"))

;; Expressions are composed recursively as strings.
;; Constructor/function names use a global translation (to enable
;; namespace prefix), and argument lists are translated (to allow for
;; data context prefix).

(define name-context (make-parameter "ex"))
(define (map-name x)
  (let ((ctx (name-context)))
    (if ctx (format "~a_~a" (name-context) x) x)))
(define (map-application lst)
  (let ((ctx (name-context)))
    (if ctx (cons ctx lst) lst)))

(define (expression x)
  (if (list? x)
      (format "~a(~a)"
              (map-name (car x))
              (string-append*
               (list->args
                (map-application
                 (map expression (cdr x))))))
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


(block ((a 123)
        (b 345))
  (block ((c (plus a b))
          (d (min a)))
     (return (foo a b))))