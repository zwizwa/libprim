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
(define map1
  (lambda (fn lst)
    (if (null? lst) lst
        (cons (fn (car lst))
              (map1 fn (cdr lst))))))
(define mapn
  (lambda (fn lsts)
    (if (null? (car lsts)) ;; assume all same length
        ()
        (cons (apply fn (map1 car lsts))
              (mapn fn (map1 cdr lsts))))))
(define map
  (lambda (fn . lsts)
    (mapn fn lsts)))
        

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
     (map1 car (cadr form))
     (map1 cadr (cadr form))
     (cddr form))))

(define-macro letrec
  (lambda (form)
    (let ((bindings (cadr form)))
      (let ((names (map1 car bindings))
            (values (map1 cadr bindings)))
        (list* 'let (map1 (lambda (n) (list n 0)) names)
               (cons 'begin (map (lambda (n v) (list 'set! n v)) names values))
               (cddr form))))))
            


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


(define (words) (map1 car (toplevel)))
(define (macro) (map1 car (toplevel-macro)))

(define-macro (cond form)
  (letrec
      ((next
        (lambda (f)
          (let ((c  (car f)))
            (if (eq? 'else (car c))
                (cadr c)
                (list 'if (car c) (cadr c)
                      (next (cdr f))))))))
    (next (cdr form))))

              
        


(post 'init-OK)
