;; Compile basic VM opcodes in s-expression form into byte code.


;;                    (let* (bindings)
;;                      (let ((body (cddr form)))
;;                        (cond
;;                         ((null? bindings) (compile `(begin ,@body)))
;;                         ((null? (cdr bindings)) (compile `(%let ,@(car bindings) `(begin ,@body))))
;;                         (else (compile `(%let ,@(car bindings) (let* ,@(cdr bindings) ,@body)))))))
                   


(define (vm-compile form)
  (define *gensym-count* 0)
  (define (gensym)
    (set! *gensym-count* (add1 *gensym-count*))
    (string->symbol
     (string-append "g" (number->string *gensym-count*))))
  (define (memoize-var x)
    (if (symbol? x) x (gensym)))

  (let compile ((form form)
                (env '())
                (menv '()))
    (define (name->index name)
      (let find ((env env)
                 (indx 0))
        (when (null? env) (undefined name))
        (if (eq? name (car env))
            indx
            (find (cdr env) (add1 indx)))))
    (define (comp e) (compile e env menv))

    (cond
     ;; Variable reference
     ((symbol? form)
      (op-ref (name->index form)))
     ;; Expression
     ((pair? form)
      (let ((tag (car form))
            (args (cdr form)))
        (cond
         ;; Application in terms of variable references
         ((eq? tag '%app)
          (let ((ids (map name->index (car args))))
            (op-app (car ids) (list->vector (cdr ids)))))

         ;; Abstraction
         ((eq? tag 'lambda)
          (let ((formals (car args))
                (body    (cdr args)))
            (op-lambda (compile `(begin ,@body)
                                (append formals env) menv)
                       (* 2 (length formals)))))

         ;; Literal values
         ((eq? tag 'quote)
          (op-lit (car args)))

         ;; Variable definition
         ((eq? tag '%let)
          (let ((name (car args))
                (expr (cadr args))
                (body (caddr args)))
            (op-let1
             (comp expr)
             (compile body (cons name env) menv))))

         ;; Sequencing
         ((eq? tag '%seq)
          (apply op-seq (map comp args)))
         ((eq? tag 'begin)
          (cond
           ((null? args) (comp '(void)))
           ((null? (cdr args)) (comp (car args)))
           (else (comp `(%seq ,(car args) (begin ,@(cdr args)))))))
         

         ;; Application: convert to nested %let with inner %app
         (else 
          (let ((vars (map memoize-var form)))
            (comp
             (let down ((v vars)
                        (f form))
               (cond 
                ((null? v) `(%app ,vars))
                ((symbol? (car f)) ;; already variable
                 (down (cdr v) (cdr f)))
                (else ;; cache in variable
                 `(%let ,(car v) 
                        ,(car f) 
                        ,(down (cdr v)
                               (cdr f))))))))))))
     ;; Constant
     (else
      (compile (list 'quote form) env menv)))))

(define (vm-eval expr)
  (vm-init (vm-compile expr))
  (vm-continue))
