;; Compile basic VM opcodes in s-expression form into byte code.


;;                    (let* (bindings)
;;                      (let ((body (cddr form)))
;;                        (cond
;;                         ((null? bindings) (compile `(begin ,@body)))
;;                         ((null? (cdr bindings)) (compile `(%let ,@(car bindings) `(begin ,@body))))
;;                         (else (compile `(%let ,@(car bindings) (let* ,@(cdr bindings) ,@body)))))))
                   


(define (vm-compile form)
  ;; Create symbols unique to this invocation of vm-compile.
  ;; FIXME: make these uninterned.
  (define *gensym-count* 0)
  (define (gensym . _)
    (set! *gensym-count* (add1 *gensym-count*))
    (string->symbol
     (string-append "#" (number->string *gensym-count*))))

  ;; Create a new symbol if a value is not a variable reference.
  (define (memoize-var x)
    (if (symbol? x) x (gensym)))

  ;; Recursive compilation, keeping track of the environment.
  (let compile ((form form)
                (env '())
                (renames '()))

    ;; To implement `let' in terms of `let*' we use renames.
    (define (map-renamed name)
      (let ((record (assq name renames)))
        (if record (cdr record) name)))
    (define (name->index name)
      (let ((renamed-name (map-renamed name)))
        (let find ((env env)
                   (indx 0))
          (when (null? env) (undefined name))
          (if (eq? renamed-name (car env))
              indx
              (find (cdr env) (add1 indx))))))
    
    (define (comp e) (compile e env renames))
    (define (comp/e e env) (compile e env renames))

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
            (op-lambda (comp/e `(begin ,@body)
                               (append formals env))
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
             (comp/e body (cons name env)))))

         ((eq? tag 'let)
          (let ((bindings (car args))
                (body (cdr args)))
            (let ((names (map car bindings))
                  (exprs (map cadr bindings)))
              (op-let
               (reverse (map comp exprs))
               (comp/e `(begin ,@body)
                       (append names env))))))

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
      (comp (list 'quote form))))))

(define (vm-eval expr)
  (vm-init (vm-compile expr))
  (vm-continue))
