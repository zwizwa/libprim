;; Compile basic VM opcodes in s-expression form into byte code.



(define (vm-compile form macros)
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
                (env '()))

    ;; To implement `let' in terms of `let*' we use renames.
    (define (name->index name)
      (let find ((env env)
                 (indx 0))
        (when (null? env) (undefined name))
        (if (eq? name (car env))
            indx
            (find (cdr env) (add1 indx)))))
    
    (define (comp e) (compile e env))
    (define (comp/e e env) (compile e env))

    (define (memo-bindings vars forms)
      (let bind ((v vars)
                 (f forms))
        (cond 
         ((null? v) '())
         ((symbol? (car f)) ;; already variable
          (bind (cdr v) (cdr f)))
         (else ;; cache in variable
          (cons (list (car v) (car f))
                (bind (cdr v) (cdr f)))))))
    
    (cond
     ;; Variable reference
     ((symbol? form)
      (op-ref (name->index form)))
     ;; Expression
     ((pair? form)
      (let ((tag (car form))
            (args (cdr form)))
        (cond
         ;; Assignment
         ((eq? tag '%set!)
          (apply op-assign (map name->index args)))
         ((eq? tag 'set!)
          (let ((name (car args))
                (expr (cadr args)))
            (let ((var (memoize-var expr)))
              (comp `(let ,(memo-bindings (list var) (list expr))
                       (%set! ,name ,var))))))
         
         ;; Abstraction
         ((eq? tag 'lambda)
          (let ((formals (car args))
                (body    (cdr args)))
            (let scan ((f formals)
                       (rnamed '()))
              (if (pair? f)
                  (scan (cdr f)
                        (cons (car f) rnamed))
                  (let ((named (reverse rnamed))
                        (rest (if (null? f) '() (list f))))
                    (op-lambda (comp/e `(begin ,@body)
                                       (append named rest env))
                               ;; LSB = have-rest-arg
                               (+ (length rest)
                                  (* 2 (length named)))))))))

         ;; Literal values
         ((eq? tag 'quote)
          (op-lit (car args)))

         ;; Variable definition
         ((eq? tag 'let)
          (let ((bindings (car args))
                (body (cdr args)))
            (if (null? bindings)
                (comp `(begin ,@body))
                (let ((names (map car bindings))
                      (exprs (map cadr bindings)))
                  (op-let
                   (reverse (map comp exprs))
                   (comp/e `(begin ,@body)
                           (append names env)))))))

         ;; Sequencing
         ((eq? tag '%seq)
          (apply op-seq (map comp args)))
         ((eq? tag 'begin)
          (cond
           ((null? args) (comp '(void)))
           ((null? (cdr args)) (comp (car args)))
           (else (comp `(%seq ,(car args) (begin ,@(cdr args)))))))

         ((eq? tag '%app)
          (let ((ids (map name->index (car args))))
            (op-app (car ids) (list->vector (cdr ids)))))

         ;; Form: macro or application.
         (else
          (let ((mrec (assq tag macros)))
            (if mrec
                (comp ((cdr mrec) form))
                ;; Application: perform evaluation using a `let' form
                ;; and pass the result to `%app' which expects values.
                (let ((vars (map memoize-var form)))
                  (comp `(let ,(memo-bindings vars form) (%app ,vars)))))))
         )))
         
     ;; Constant
     (else
      (comp (list 'quote form))))))

(define (vm-eval expr)
  (vm-init (vm-compile expr))
  (vm-continue))
