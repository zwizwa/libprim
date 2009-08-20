#lang scheme/base

;; Simple prettyprinter.

(require "tools.ss")
(provide (all-defined-out))

;; State
(define pp-column (make-parameter 0))  ;; current column
(define pp-margin (make-parameter 0))

;; Configurable behaviour
(define _pp-display (make-parameter display)) ;; text output function
(define pp-indent-amount (make-parameter 2))

(define (pp-add-to-margin n)
  (pp-margin (+ n (pp-margin))))

(define (pp-display str)
  (pp-column (+ (string-length str) (pp-column)))
  ((_pp-display) str))
(define (pp-enter)  ;; newline and indent
  ((_pp-display) "\n")
  (pp-column 0)
  (pp-display (pp-make-spaces (pp-margin))))


;; Goes into effect at the next `pp-enter'.
(define (pp-start-indent thunk)
  (parameterize ((pp-margin (+ (pp-indent-amount) (pp-margin))))
    (pp-enter)
    (thunk))
  (pp-enter))

(define (pp-make-spaces n)
  (string-append* (for/list ((_ (in-range n))) ".")))

(define (pp-display-add-indent str)
  (pp-display str)
  (pp-add-to-margin (string-length str)))





;; Expressions


(define (arglist lst)
  (string-append* (list->args lst)))

(define (add-type t) (lambda (x) (format "~a ~a" t x)))
(define (pointer t) (format "*~a" t))

(define name-context (make-parameter "ex"))

(define ((if-ctx fn) x)
  (let ((ctx (name-context)))
    (if ctx (fn ctx x) x)))
(define map-name (if-ctx (lambda (ctx x) (format "~a_~a" ctx x))))
(define map-app  (if-ctx (lambda (ctx lst) (cons ctx lst))))
(define map-def  (if-ctx (lambda (ctx lst)
                           (cons ((add-type ctx)
                                  (pointer ctx)) lst))))

;; Statements
(define (emit-return expr)
  (pp-display-add-indent "return ")
  (emit-expression expr)
  (pp-display ";")
  (pp-enter))

(define (emit-definition name formals expr)
  (pp-display
   (format "_ ~a(~a)"
           (map-name name)
           (arglist
            (map-def
             (map (add-type "_")
                  (map symbol->string formals))))))
  (pp-enter)
  (pp-display "{")
  (pp-start-indent
   (lambda () (emit-return expr)))
  (pp-display "}"))
   
(require mzlib/match)
(define (emit-expression expr)
  (parameterize ((pp-margin (pp-margin))) ;; save-excursion
    (match
     expr
     (('let* bindings body)
      (begin
        (pp-display-add-indent "({ ")
        (for ((b bindings))
          (apply emit-binding b)
          (pp-enter))
        (emit-expression body)))
     ((fn . args)
      (pp-display-add-indent
       (format "~a(" (map-name fn)))
      (let loop ((a (map-app args)))
        (if (null? a)
            (pp-display ")")
            (begin
              (emit-expression (car a))
              (unless (null? (cdr a))
                (pp-display ","))
              (loop (cdr a))))))
     (else
      (pp-display (format "~a" expr))))))

(define (emit-binding var expr)
  (pp-display-add-indent
   (format "_ ~a = " var))
  (emit-expression expr))

(emit-definition 'foo '(a b c) '(foo a b))
(pp-enter)

(emit-expression '(foo a b))
(pp-enter)