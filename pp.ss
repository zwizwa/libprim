#lang scheme/base

;; Simple prettyprinter for semi-scheme -> C compilation.

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
  (string-append* (for/list ((_ (in-range n))) " ")))

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

(require mzlib/match)

;; Statements
(define (emit-return expr)
  (pp-display-add-indent "return ")
  (emit-expression expr)
  (pp-display ";"))

(define emit-definition
  (match-lambda
   (('define (name . formals) expr)
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
    (pp-display "}"))))

;; Expressions

(define-syntax-rule (pp-save . body)
  (parameterize ((pp-margin (pp-margin))) ;; save-excursion
    . body))

(define (emit-expression expr)
  (pp-save
    (match
     expr
     (('let* bindings . body)
      (begin
        (pp-display-add-indent "({ ")
        (for ((b bindings))
          (apply emit-binding b)
          (pp-enter))
        (let loop ((e body))
          (if (null? e)
              (pp-display "})")
              (begin
                (emit-expression (car e))
                (pp-display ";")
                (unless (null? (cdr e)) (pp-enter))
                (loop (cdr e)))))))
     ((fn . args)
      (pp-display-add-indent
       (format "~a(" (map-name fn)))
      (let loop ((a (map-app args)))
        (if (null? a)
            (pp-display ")")
            (begin
              (emit-expression (car a))
              (unless (null? (cdr a))
                (pp-display ", ")
                ;; Do this contitionally, using backtracking?
                ;; I.e. fail when a subexpression contains multiple
                ;; lines, or when it wraps over the edge.

                ;; (pp-enter)
                )
              (loop (cdr a))))))
     (else
      (pp-display (format "~a" expr))))))

(define (emit-binding var expr)
  (pp-save
   (pp-display-add-indent
    (format "_ ~a = " var))
   (emit-expression expr)
   (pp-display ";")))


(define (compile-file file)
  (with-input-from-file file
    (lambda ()
      (let loop ()
        (let ((expr (read)))
          (when (not (eof-object? expr))
            (emit-definition expr)
            (pp-enter)
            (loop)))))))
