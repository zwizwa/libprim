;;; Bootstrap

;; This is boot phase 1 for libprim/sc VM, which is incrementally
;; bootstrapped from the C source. Phase 2 is shared with other VM,
;; which is batch-bootstrapped from Scheme.


;; A single s-expression will be allocated outside the VM and passed
;; to the interpretation step.  This means there is no GC during
;; construction as long as it fits in the initial cell store: see
;; _sc_init() in scheme.c
(begin

;; Macro expander + support.
(def-toplevel! 'assq
  (lambda (obj lst)
    (if (null? lst) #f
        (if (eq? (caar lst) obj) (car lst)
            (assq obj (cdr lst))))))
(def-toplevel! 'map1
  (lambda (fn lst)
    (if (null? lst) lst
        (cons (fn (car lst))
              (map1 fn (cdr lst))))))
(def-toplevel! 'expand-lambda
  (lambda (expr)
    (cons 'lambda
    (cons (cadr expr)
    (map1 expand (cddr expr))))))
(def-toplevel! 'expand
  (lambda (expr)
    (if (pair? expr)
        ((lambda (tag)
           (if (eq? tag 'quote) expr
           (if (eq? tag 'lambda) (expand-lambda expr)
               ((lambda (rec)
                  (if rec
                      (expand ((cdr rec) expr))
                      (map1 expand expr)))
                (assq (car expr) (toplevel-macro))))))
         (car expr))
        expr)))
;; Implemented in terms of primitive continuation transformers (ktx).
(def-toplevel! 'eval
  (lambda (expr) (letcc k ((eval-ktx k (expand expr))))))

;; Evaluate expressions in sequence.  This makes sure macros take
;; effect immediately after definition.
(def-toplevel! 'eval-list
  (lambda (expr)
    (if (null? expr) (void)
        (begin (eval (car expr)) (eval-list (cdr expr))))))

;;; Library with macro expansion.

;; The rest is evaluated in sequence with `eval' defined above, which
;; also performs macro expansion.
(eval-list '(

(def-toplevel! 'list (lambda args args))

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
(define-macro let
  (lambda (form)
    ((lambda (names values body)
       (list* (list* 'lambda names body) values))
     (map1 car (cadr form))
     (map1 cadr (cadr form))
     (cddr form))))

(define-macro letrec
  (lambda (form)
    (let ((bindings (cadr form)))
      (let ((names (map1 car bindings))
            (values (map1 cadr bindings)))
        (list* 'let (map1 (lambda (n) (list n #f)) names)
               (cons 'begin (map (lambda (n v) (list 'set! n v)) names values))
               (cddr form))))))

;; Overwrite define + define-macro with more complete implementations.

;; Convert define syntax into (list symbol form)
(define expand-define
  (lambda (form)
    (let ((name (cadr form))
          (value (caddr form)))
      (if (pair? name)
          (let ((_name (car name))
                (_formals (cdr name)))
            (set! name _name)
            (set! value (list* 'lambda _formals (cddr form)))))
      (list name value))))
(define make-definer
  (lambda (def!)
    (lambda (form)
      (let ((n+v (expand-define form)))
        (list def! (list 'quote (car n+v)) (cadr n+v))))))
(define-macro define (make-definer 'def-toplevel!))
(define-macro define-macro (make-definer 'def-toplevel-macro!))

(define (vars) (map1 car (toplevel)))
(define (macros) (map1 car (toplevel-macro)))

(define (with-letform-transpose bindings_body fn)
  (fn (map1 car (car bindings_body))
      (map1 cadr (car bindings_body))
      (cdr bindings_body)))

;; Redefine let with named let.
(define-macro (let form)
  (if (symbol? (cadr form))
      ;; named let
      (with-letform-transpose
       (cddr form)
       (lambda (names values body)
         (let ((name (cadr form)))
           (list 'letrec
                 (list (list name (list* 'lambda names body)))
                 (cons name values)))))
      ;; normal let
      (with-letform-transpose
       (cdr form)
       (lambda (names values body)
         (list* (list* 'lambda names body) values)))))

(%load "boot2.scm")
)))

