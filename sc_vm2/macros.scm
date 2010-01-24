(define-macro quasiquote
  (lambda (l)
    (letrec
        ((mcons
          (lambda (f l r)
            (if (and (pair? r)
                     (eq? (car r) 'quote)
                     (eq? (car (cdr r)) (cdr f))
                     (pair? l)
                     (eq? (car l) 'quote)
                     (eq? (car (cdr l)) (car f)))
                (if (or (procedure? f) (number? f) (string? f))
                    f (list 'quote f))
                                        ;(if (eqv? l vector)
                                        ;    (apply l (eval r))
                (list 'cons l r)
                )))
         (mappend
          (lambda (f l r)
            (if (or (null? (cdr f))
                    (and (pair? r)
                         (eq? (car r) 'quote)
                         (eq? (car (cdr r)) '())))
                l (list 'append l r))))
         (tx
          (lambda (level form)
            (cond ((not (pair? form))
                   (if (or (procedure? form) (number? form) (string? form))
                       form (list 'quote form)))
                  ((eq? 'quasiquote (car form))
                   (mcons form ''quasiquote (tx (+ level 1) (cdr form))))
                  (#t (if (zero? level)
                          (cond ((eq? (car form) 'unquote) (car (cdr form)))
                                ((eq? (car form) 'unquote-splicing)
                                 (error "Unquote-splicing wasn't in a list" form))
                                ((and (pair? (car form))
                                      (eq? (car (car form)) 'unquote-splicing))
                                 (mappend form (car (cdr (car form)))
                                          (tx level (cdr form))))
                                (#t (mcons form (tx level (car form))
                                           (tx level (cdr form)))))
                          (cond ((eq? (car form) 'unquote)
                                 (mcons form ''unquote (tx (- level 1) (cdr form))))
                                ((eq? (car form) 'unquote-splicing)
                                 (mcons form ''unquote-splicing
                                        (tx (- level 1) (cdr form))))
                                (#t (mcons form (tx level (car form))
                                           (tx level (cdr form)))))))))))
      (tx 0 (car (cdr l))))))


(define-macro cond
  (lambda (form)
    (let next ((clauses (cdr form)))
      (if (null? clauses) '(void)
          (let* ((clause (car clauses))
                 (rest (lambda () (next (cdr clauses))))
                 (guard (car clause))
                 (body (cons 'begin (cdr clause))))
            (cond
             ((eq? 'else guard) body)
             ((null? (cdr clause))
              `(if ,guard ,(void) ,(rest)))
             ((eq? '=> (cadr clause))
              (let ((body (caddr clause)))
                `(let ((bv ,guard))
                   (if bv (,body bv) ,(rest)))))
             (else
              `(if ,guard ,body ,(rest)))))))))


(define-macro or
  (lambda (form)
    (let clause ((args (cdr form)))
      (if (null? args) #t
          (if (null? (cdr args)) (car args)
              (list 'if (car args)
                    (car args)
                    (clause (cdr args))))))))

(define-macro and
  (lambda (form)
    (let clause ((args (cdr form)))
      (if (null? args) '#f
          (if (null? (cdr args)) (car args)
              (list 'if (car args) 
                    (clause (cdr args))
                    (car args)))))))

(define-macro lambda
  (lambda (form)
    (define expand-define
      (lambda (form)
        (let ((name (cadr form))
              (value (caddr form)))
          (if (pair? name)
              (let ((_name (car name))
                    (_formals (cdr name)))
                (set! name _name)
                (set! value (list* 'lambda _formals (cddr form))))
              #f)
          (list name value))))
    (define make-definer
      (lambda (def!)
        (lambda (form)
          (let ((n+v (expand-define form)))
            (list def! (list 'quote (car n+v)) (cadr n+v))))))
    (let ((define?
            (lambda (x)
              (if (pair? x)
                  (if (pair? (car x)) (eq? 'define (caar x)) #f)
                  #f))))
      (let collect ((body (cddr lambda-form)) (defs '()))
        (if (define? body)
            (collect (cdr body) (cons (expand-define (car body)) defs))
            (let ((defs (reverse defs)))
              (list*
               '%lambda
               (cadr lambda-form)
               (if (null? defs)
                   (map expand body)
                   (list (expand (list* 'letrec (reverse defs) body)))))))))))
