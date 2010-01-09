;;; Bootstrap

;; This is boot phase 1 for libprim/sc VM, which is incrementally
;; bootstrapped from the C source.  Most code is shared with the other
;; VM.

;; Note that this file, and the files it loads before boot2.scm are
;; quite brittle due to incremental bootstrapping.  Be careful when
;; you change anything.

;;;

;; A single s-expression will be allocated outside the VM and passed
;; to the interpretation step.  This means there is no GC during
;; construction as long as it fits in the initial cell store: see
;; _sc_init() in scheme.c
(begin

;; Macro expander + support.
(def-toplevel! 'assq
  (%lambda (obj lst)
    (if (null? lst) #f
        (if (eq? (caar lst) obj) (car lst)
            (assq obj (cdr lst))))))
(def-toplevel! 'map1
  (%lambda (fn lst)
    (if (null? lst) lst
        (cons (fn (car lst))
              (map1 fn (cdr lst))))))
(def-toplevel! 'expand-%lambda
  (%lambda (expr)
    (cons '%lambda
    (cons (cadr expr)
    (map1 expand (cddr expr))))))
(def-toplevel! 'expand
  (%lambda (expr)
    (if (pair? expr)
        ((%lambda (tag)
           (if (eq? tag 'quote) expr
           (if (eq? tag '%lambda) (expand-%lambda expr)
               ((%lambda (rec)
                  (if rec
                      (expand ((cdr rec) expr))
                      (map1 expand expr)))
                (assq (car expr) (toplevel-macro))))))
         (car expr))
        expr)))
;; Implemented in terms of primitive continuation transformers (ktx).
(def-toplevel! 'eval
  (%lambda (expr) (letcc k ((eval-ktx k (expand expr))))))

;; Evaluate expressions in sequence.  This makes sure macros take
;; effect immediately after definition.
(def-toplevel! 'eval-list
  (%lambda (expr)
    (if (null? expr) (void)
        (begin (eval (car expr)) (eval-list (cdr expr))))))

;;; Library with macro expansion.

;; The rest is evaluated in sequence with `eval' defined above, which
;; also performs macro expansion.
(eval-list '(

;; Start with plain lambda.
(def-toplevel-macro! 'lambda
  (%lambda (form) (cons '%lambda (cdr form))))
             
(def-toplevel! 'list
  (lambda args args))

(def-toplevel-macro!
  'define
  (lambda (form)
    (list 'def-toplevel!
          (list 'quote (car (cdr form)))
          (car (cdr (cdr form))))))
(def-toplevel-macro!
  'define-macro
  (lambda (form)
    (list 'def-toplevel-macro!
          (list 'quote (cadr form))
          (caddr form))))

(define apply1 (lambda (fn args) (letcc k ((apply-ktx k args) fn))))
(define apply apply1)


(define mapn
  (lambda (fn lsts)
    (if (null? (car lsts)) ;; assume all same length
        '()
        ((lambda (head) ;; (*)
           (cons head (mapn fn (map1-prim cdr lsts))))
         (apply1 fn (map1-prim car lsts))))))

;; (*) to implement `for-each' in terms of `map', it's simplest to
;; make sure that `fn' in `mapn' is applied from left to right.  Note
;; that all application forms (like `cons' above) evaluate from right
;; to left.

(define map
  (lambda (fn . lsts)
    (mapn fn lsts)))

(define list* (lambda (a . rest)
                (if (null? rest) a
                    (cons a (apply list* rest)))))
;; This let is in terms of lambda.
(define expand-%let
  (lambda (form)
    ((lambda (names values body)
       (list* (list* 'lambda names body) values))
     (map1 car (cadr form))
     (map1 cadr (cadr form))
     (cddr form))))

(define-macro let expand-%let)
(define-macro %let expand-%let)

(%load "boot1-expand-letrec.scm")
(define-macro letrec expand-letrec)

;; Overwrite define + define-macro with more complete implementations.
(%load "boot1-expand-define.scm")
(define-macro define (make-definer 'def-toplevel!))
(define-macro define-macro (make-definer 'def-toplevel-macro!))

;; Redefine let with named let.
(%load "boot1-expand-let.scm")
(define-macro let expand-let)

;; Support internal definitions
(%load "boot1-expand-lambda.scm")
(define-macro lambda expand-lambda)

(%load "boot2.scm")
)))

