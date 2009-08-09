;; Bootstrap
(def-toplevel! 'list (lambda args args))
(def-toplevel-macro!
  'define
  (lambda (form)
    (list 'def-toplevel!
          (list 'quote (car (cdr form)))
          (car (cdr (cdr form))))))
;; (define dbg (lambda (x) (post x) x))
(define cadr  (lambda (x) (car (cdr x))))
(define cddr  (lambda (x) (cdr (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(def-toplevel-macro!
  'define-syntax
  (lambda (form)
    (list 'def-toplevel-macro!
          (list 'quote (cadr form))
          (caddr form))))
(define map (lambda (fn lst)
              (if (null? lst) lst
                  (cons (fn (car lst))
                        (map fn (cdr lst))))))
(define words (lambda () (map car (toplevel))))
(define macros (lambda () (map car (toplevel-macro))))
(define-syntax let
  (lambda (form)
    ((lambda (names values body)
       (cons (cons 'lambda (cons names body)) values))
     (map car (cadr form))
     (map cadr (cadr form))
     (cddr form))))
(define make-definer
  (lambda (def!)
    (lambda (form)
      (let ((name (cadr form))
            (value (caddr form)))
        ;; (define (_name . _formals) . body)
        (if (pair? name)
            (let ((_name (car name))
                  (_formals (cdr name)))
              (set! name _name)
              (set! value (cons 'lambda
                                (cons _formals
                                      (cddr form))))))
        (list def! (list 'quote name) value)))))

;; Overwrite define + define-syntax with more complete implementations.
(define-syntax define (make-definer 'def-toplevel!))
(define-syntax define-syntax (make-definer 'def-toplevel-macro!))

;; Implemented in terms of continuation transformers (ktx).
(define (apply fn args) (letcc k ((apply-ktx k fn args))))
(define (eval expr) (letcc k ((eval-ktx k expr))))




(post 'init-OK)
