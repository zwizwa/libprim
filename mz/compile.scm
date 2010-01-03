;; Compile basic VM opcodes in s-expression form into byte code.



(define vm-compile
(lambda (form macros)
  ;; Create unique variable tags.  The numbering is only for debugging.
;;   (define *count* 0)
;;   (define (genvar)
;;     (set! *count* (add1 *count*))
;;     (cons 'g *count*))
  (define (genvar)
    (cons 'g '()))
  (define (varref? x)
    (or (symbol? x)
        (and (pair? x)
             (eq? (car x) 'g))))
  ;; Create a new symbol if a value is not a variable reference.
  (define (memoize-var x)
    (if (varref? x) x (genvar)))
  ;; Recursive compilation, keeping track of the environment.
  (let compile ((form form)
                (env '()))
    (define (comp e) (compile e env))
    (define (comp/e e env) (compile e env))
    (define (name->index name)
      (let find ((env env)
                 (indx 0))
        (when (null? env) (undefined name))
        (if (eq? name (car env))
            indx
            (find (cdr env) (add1 indx)))))
    ;; For conversion to ANF: add binding sites for expressions -
    ;; ignore varrefs.
    (define (memo-bindings vars forms)
      (let bind ((v vars)
                 (f forms))
        (cond 
         ((null? v) '())
         ((varref? (car f)) ;; already variable
          (bind (cdr v) (cdr f)))
         (else ;; cache in variable
          (cons (list (car v) (car f))
                (bind (cdr v) (cdr f)))))))
    
    (cond
     ;; Variable reference
     ((varref? form)
      (op-ref (name->index form)))
     ;; Expression
     ((pair? form)
      (let ((tag (car form))
            (args (cdr form)))
        (cond
         ;; Macros.  Check these first to keep compiler extensible.
         ((assq tag macros) => (lambda (mrec)
                                 (comp ((cdr mrec) form))))
         
         ;; Conditional
         ((eq? tag '%ifval)
          (let ((cvar (car args))
                (branches (cdr args)))
            (apply op-if (name->index cvar) (map comp branches))))
         ((eq? tag 'if)
          (let ((cexp (car args))
                (branches (cdr args)))
            (let ((cvar (memoize-var cexp)))
              (comp `(let ,(memo-bindings (list cvar) (list cexp))2
                       (%ifval ,cvar ,@branches))))))

         ;; Assignment
         ((eq? tag '%setval!)
          (apply op-assign (map name->index args)))
         ((eq? tag 'set!) ;; allow macro override
          (comp `(%set! ,@args)))
         ((eq? tag '%set!)
          (let ((name (car args))
                (expr (cadr args)))
            (let ((var (memoize-var expr)))
              (comp `(let ,(memo-bindings (list var) (list expr))
                       (%setval! ,name ,var))))))
         
         ;; Abstraction
         ((eq? tag 'lambda) ;; allow macro override
          (comp `(%lambda ,@args)))
         ((eq? tag '%lambda)
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
         ((eq? tag 'let) ;; allow macro override
          (comp `(%let ,@args)))
         ((eq? tag '%let)
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

         ;; Application: perform evaluation using a `let' form
         ;; and pass the result to `%app' which expects values.
         ((eq? tag '%app)
          (let ((ids (map name->index (car args))))
            (op-app (car ids) (list->vector (cdr ids)))))
         (else
          (let ((vars (map memoize-var form)))
            (comp `(let ,(memo-bindings vars form) (%app ,vars)))))
         )))
         
     ;; Constant
     (else
      (comp (list 'quote form)))))))


(define (vm-eval expr)
  (vm-init (vm-compile expr '()))
  (vm-continue))
