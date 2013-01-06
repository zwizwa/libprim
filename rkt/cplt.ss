#lang scheme/base


(require (planet dherman/c:3:2)
         scheme/control
         scheme/pretty
         scheme/match)



(require (for-syntax scheme/base))

;; Some simplified pattern matching syntax, ignoring the source
;; location, with implicit `struct' and one level of parens removed.

(define-syntax (cmatch stx)
  (define (tx-pattern stx)
    (syntax-case stx ()
      ((tag . args)
       #`(struct tag (src #,@(map tx-pattern (syntax->list #'args)))))
      (var #'var)))
  (syntax-case stx ()
    ((_ in (pat expr) ...)
     #`(match in
              #,@(for/list ((p (syntax->list #'(pat ...)))
                            (e (syntax->list #'(expr ...))))
                   (list (tx-pattern p) e))
              (error 'cmatch "~s" in)))))


;; Naive pretty-printing:  Expressions are printed in nested form
;; on one line.  Statements are printed one per line.  Each block
;; indents.  All built-in operations have explicit parenthesis to
;; avoid precedence problems.

(define (ast-emit ast [emit display] [tab-spacing "  "])
  (define (string x) (emit (format "~a" x)))
  (define-syntax emit/call
    (syntax-rules ()
      ((_ (op . args)) (op . args))
      ((_ datum) (emit datum))))
  (define-syntax-rule (e: it ...)
    (begin (emit/call it) ...))
  (let down ((ast ast) (tab ""))
    ;; `es' means emit substructure
    (define (es x) (down x tab))  
    (define (es/tab x) (down x (string-append tab-spacing tab)))
    (define (es/comma lst)
      (let ((n (length lst)))
        (when (> n 0)
          (es (car lst))
          (when (> n 1)
            (for ((l (cdr lst)))
              (emit ", ")
              (es l))))))
    (cond
     ((symbol? ast) (emit (symbol->string ast)))
     ((list? ast) (for-each es ast))
     ((not ast) (void))
     (else
      (cmatch
       ast

       ((id:var name)           (es name))
       ((id:op  name)           (es name))
       ((type:primitive name)   (es name))
       ((expr:ref id)           (es id))
       ((expr:call fun args)    (e: (es fun) "(" (es/comma args) ")"))
       
       ((stmt:block items)      (e: tab "{\n" (es/tab items) tab "}\n"))
       ((stmt:expr expr)        (e: tab (es expr) ";\n"))
       ((expr:binop l op r)     (e: "(" (es l) " " (es op) " " (es r) ")"))
       ((type:function r fs)    (e: (es r) "(" (es/comma fs) ")"))

       ((init:expr init)        (e: " = " (es init)))
       
       ((decl:declarator i t =) (e: (es i) (es t) (es =)))
       ((decl:formal _ t d)     (e: (es t) " " (es d)))

       ((decl:vars _ t d)       (e: tab (es t) " " (es/comma d) ";\n"))
       ((decl:function
         _ _ rt dec _ body)     (e: (es rt) " " (es dec) " " (es body)))
       
       )))))


(define (codegen)
  (define src #f)
  (define (function type name formals body)
    (make-decl:function src #f #f
                        type
                        (declarator
                         (variable name)
                         (make-type:function src #f formals))
                        #f body))
  (define (formal type name)
    (make-decl:formal src #f type (declarator (variable name))))
  (define (declarator id [type #f] [init #f])
    (make-decl:declarator src id type init))

  (define (block lst) (make-stmt:block src lst))
  (define (variable name) (make-id:var src name))
  
  (define (primitive-type sym) (make-type:primitive src sym))
  (define void (primitive-type 'void))
  (define int (primitive-type 'int))
  (define _ (primitive-type '_))  ;; FIXME

  ;; Dynamic types (libprim/pf)
  (define (function/dt name fs body)
    (function _ name (for/list ((f fs)) (formal _ f)) body))

  (function/dt 'foo '(a b) (block '())))
                      

;; (ast-emit (codegen))


(parse-program (current-input-port))

