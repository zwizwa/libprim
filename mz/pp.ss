#lang scheme/base

;; Simple prettyprinter for semi-scheme -> C compilation.

;; This produces code in the EX language, a C-based first-order
;; dynamically typed language using Scheme's data types + C's control
;; types (i.e. no proper tail calls).

(require scheme/dict
         scheme/pretty
         "../tools.ss"
         "mangle.ss")
(provide (all-defined-out))

;; SIMPLE INDENTING PRETTY PRINTER

;; State
(define pp-column (make-parameter 0))  ;; current column
(define pp-margin (make-parameter 0))

;; Configurable behaviour
(define _pp-display (make-parameter display)) ;; text output function
(define pp-indent-amount (make-parameter 4))

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
  ;; (pp-enter)
  )

(define (pp-make-spaces n)
  (string-append* (for/list ((_ (in-range n))) " ")))

(define (pp-display-add-indent str)
  (pp-display str)
  (pp-add-to-margin (string-length str)))





;; PRIMITIVE EXPRESSION TRANSLATION

;; Scheme -> C function call mapping. I.e. :
;; (foo a b c)              -> ex_foo(ex, a, b, c)
;; (define (foo a b c) ...) -> ex_foo(ex *ex, a, b, c)
(define name-context (make-parameter "ex"))
(define (arglist lst)
  (string-append* (list->args lst)))
(define (add-type t) (lambda (x) (format "~a ~a" t x)))
(define (pointer t) (format "*~a" t))
(define ((if-ctx fn) x)
  (let ((ctx (name-context)))
    (if ctx (fn ctx x) x)))
;; * Function name prefixing
(define map-name (if-ctx (lambda (ctx x)
                           (scheme->c (symbol->string x)
                                      (string-append ctx "_")))))
;; * Prefixing a context variable to the argument list of function
;; applications.
(define map-app  (if-ctx (lambda (ctx lst)
                           (cons ctx lst))))
;; * Prefixing and transforming function definitions
(define map-def  (if-ctx (lambda (ctx lst)
                           (cons ((add-type ctx)
                                  (pointer ctx)) lst))))

(require mzlib/match)


;; CORE EXPRESSION LANGUAGE TRANSLATION

;; Conversion from a first-order Scheme subset (`if', `let*', variable
;; reference and function application) to C code using the EX naming
;; and calling conventions.

(define (emit-return expr)
  ;; (pp-display-add-indent "return ")
  (pp-display "return") (pp-enter)
  (emit-expression expr)
  (pp-display ";"))

(define (pp-comment expr)
  (let ((lines 
         (for/list ((l (in-lines
                        (open-input-string
                         (pretty-format expr 60))))) l)))
    (display "/* ")
    (display (car lines))
    (for ((l (cdr lines)))
      (display "\n   ")
      (display l))
    (display " */\n")))


(define (emit-definition expr)
  (match expr
   (('define (name . formals) expr)
    (pp-enter)
    (pp-comment expr)
    (pp-display
     (format "_ ~a(~a)"
             (map-name name)
             (arglist
              (map-def
               (map (add-type "_")
                    (map symbol->string formals))))))
    ;; (pp-enter)
    (pp-display " {")
    (pp-start-indent
     (lambda () (emit-return (ex-expand expr))))
    (pp-enter)
    (pp-display "}"))))



(define-syntax-rule (pp-save . body)
  (parameterize ((pp-margin (pp-margin))) ;; save-excursion
    . body))
(define (emit-binding var expr)
  (pp-save
   (pp-display-add-indent
    (format "_ ~a = " var))
   (emit-expression expr)
   (pp-display ";")))

(define (emit-expression expr)
  (define N pp-enter)
  (define D pp-display)
  (define D/ pp-display-add-indent)
  (define E emit-expression)
  (define (err [e expr]) (error 'ex-syntax-error e))
  (pp-save
    (match
     expr

     ;; CONSTANTS
     (#f (D "FALSE"))
     (#t (D "TRUE"))

     ;; CONDITIONAL
     (('if cond yes no)
      (D "(FALSE != ") (E cond) (D ") ?")
      (pp-start-indent
       (lambda ()
         (D "(") (E yes) (D ") :") (N)
         (D "(") (E no)  (D ")"))))
     (('if cond yes)
      (E `(if ,cond ,yes (cref "VOID"))))
     (('if . _) (err))

     ;; BLOCK (VARIABLE BINDING + SEQUENTIAL EXECUTION)
     (('let* bindings . body)
      (begin
        (D/ "({ ")
        (for ((b bindings))
          (apply emit-binding b) (N))
        (let loop ((e body))
          (if (null? e)
              (D "})")
              (begin
                (E (car e))
                (D ";")
                (unless (null? (cdr e)) (N))
                (loop (cdr e)))))))
     (('let* . _) (err))

     (('cref name)  (D (format "~a" name)))
     
     ;; FUNCTION APPLICATION
     ((fn . args)
      (D/ (format "~a(" (map-name fn)))
      (let loop ((a (map-app args)))
        (if (null? a)
            (D ")")
            (begin
              (E (car a))
              (unless (null? (cdr a))
                (D ", ")
                ;; Do this conditionally, using backtracking?
                ;; I.e. fail when a subexpression contains multiple
                ;; lines, or when it wraps over the edge.

                ;; (N)
                )
              (loop (cdr a))))))

     ;; Non-function variable references and immediates.
     (else (D (format "~a" expr))))))



;; SYNTACTIC SUGAR

(define pp-macros
  (make-parameter
   `((begin . ,(lambda (form) `(let* () ,@(cdr form)))))))

(define (ex-expand expr [macros (pp-macros)])
  (match expr
         ;; Expand through core forms.
         (('if . forms)
          `(if ,@(map ex-expand forms)))
         (('let* bindings . forms)
          `(let* ,(for/list ((b bindings))
                    (match b ((name expr) `(,name ,(ex-expand expr)))))
             ,@(map ex-expand forms)))
         ;; Expand a macro or expand through application.
         ((tag . _)
          (cond ((dict-ref macros tag #f) => (lambda (m) (m expr)))
                (else (map ex-expand expr))))
         ;; Leaf nodes (varref or immediate)
         (else expr)))
           
  


;; FRONTENDS

(define (compile-file file)
  (with-input-from-file file
    (lambda ()
      (let loop ()
        (let ((expr (read)))
          (when (not (eof-object? expr))
            (emit-definition expr)
            (pp-enter)
            (loop)))))))
;; (parameterize
;;     ((current-output-port
;;       (open-output-file "/tmp/out.c" #:exists 'append)))

  (emit-definition
   '(define (f x) (if (even? x) x #f)))
  
  (emit-definition
   '(define (f x) (let* ((a (add x x))
                         (b (mul y y)))
                    (post a)
                    (post b)
                    (if (not (zero? a)) (post a)
                        (if b (post b)))
                    (if b (post b))
                    (div a b))))
  ;;)
  

(pp-enter)
