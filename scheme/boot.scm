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
  'define-macro
  (lambda (form)
    (list 'def-toplevel-macro!
          (list 'quote (cadr form))
          (caddr form))))
(define map (lambda (fn lst)
              (if (null? lst) lst
                  (cons (fn (car lst))
                        (map fn (cdr lst))))))
;; Implemented in terms of continuation transformers (ktx).
(define apply (lambda (fn args) (letcc k ((apply-ktx k fn args)))))
(define eval  (lambda (eval expr) (letcc k ((eval-ktx k expr)))))

(define list* (lambda (a . rest)
                (if (null? rest) a
                    (cons a (apply list* rest)))))
(define-macro let
  (lambda (form)
    ((lambda (names values body)
       (list* (list* 'lambda names body) values))
     (map car (cadr form))
     (map cadr (cadr form))
     (cddr form))))

;; (define (letrec-tx form)
;;   (let ((bindings (cadr form)))
;;     (let ((names (map car bindings))
;;           (values (map cadr bindings)))
;;       (list* 'let (map (lambda (n) (list n 0)) names)
;;              (cons 'begin (map (lambda (n v) (list 'set! n v)) names values))
;;              (cddr form)))))
            


;; Overwrite define + define-macro with more complete implementations.
(define make-definer
  (lambda (def!)
    (lambda (form)
      (let ((name (cadr form))
            (value (caddr form)))
        (if (pair? name)
            (let ((_name (car name))
                  (_formals (cdr name)))
              (set! name _name)
              (set! value (list* 'lambda _formals (cddr form)))))
        (list def! (list 'quote name) value)))))
(define-macro define (make-definer 'def-toplevel!))
(define-macro define-macro (make-definer 'def-toplevel-macro!))


;; (define-macro (let form)
;;   (let ((name (cadr form)))
;;     (if (symbol? name)
;;         ;; named let
;;         ()
;;         (let-tx form))))


(define (words) (map car (toplevel)))
(define (macro) (map car (toplevel-macro)))


;; (Define (_cond form)
;;   (let ((c  (cadr form))
;;         (cs (cddr form)))
;;     (if (eq? 'else (car c))
;;         (cadr c)
;;         (cons 'if
;;               (cons (car c)
;;                     (cons (cadr c)
;;                           (cons (cond 
;;               (car c)
;;               (cadr c)
;;               (cond 
              
        


(post 'init-OK)
