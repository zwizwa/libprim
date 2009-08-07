;; Bootstrap
(def-toplevel! 'list (lambda args args))
(def-toplevel-macro!
 'define
 (lambda (form)
   (list 'def-toplevel!
         (list 'quote (car (cdr form)))
         (car (cdr (cdr form))))))
(define cadr  (lambda (x) (car (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))

(def-toplevel-macro!
  'define-syntax
  (lambda (form)
    (list 'def-toplevel-macro!
          (list 'quote (cadr form))
          (caddr form))))
