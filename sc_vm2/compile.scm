;; Compile basic VM opcodes in s-expression form into byte code.

;; This is (will be) self-hosting and run on R5RS
;;   - basic forms:  <application> <reference> if lambda quote begin let
;;   - macros: let (letrec, named-let) lambda (internal defs)

;; (load "lib.scm_") ;; expanders for: let letrec lambda define->bindings



(define vm-compile/macros
(lambda (form toplevel macros)
  ;; Create unique variable tags.  The numbering is only for debugging.
;;   (define *count* 0)
;;   (define (genvar)
;;     (set! *count* (add1 *count*))
;;     (cons 'g *count*))
  (define (op opcode)
    (lambda args (cons opcode args)))
  (define (genvar)
    (cons 'g '()))
  (define (varref? x)
    (or (symbol? x)
        (and (pair? x)
             (eq? (car x) 'g))))
  ;; Create a new symbol if a value is not a variable reference.
  (define (memoize-var x)
    (if (varref? x) x (genvar)))

  ;; Keep track of free variables.
  (define free '())
  (define (free-var! name)
    (if (memq name free) #f
        (set! free (cons name free)))
    ;; `(topref ,name)
    name)
  
  ;; Recursive compilation, keeping track of the environment.
  (define (compile form env)
    (define (comp e) (compile e env))
    (define (comp/e e env) (compile e env))
    (define (name->index name)
      (let find ((env env)
                 (indx 0))
        (if (null? env)
            (let ((rec (assq name toplevel)))
              (if rec (cdr rec)
                  (free-var! name)))
            (if (eq? name (car env))
                indx
                (find (cdr env) (add1 indx))))))
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
      ((op 'op-ref) (name->index form)))
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
            (apply (op 'op-if) (name->index cvar) (map1 comp branches))))
         ((eq? tag 'if)
          (let ((cexp (car args))
                (branches (cdr args)))
            (let ((cvar (memoize-var cexp)))
              (comp `(let ,(memo-bindings (list cvar) (list cexp))2
                       (%ifval ,cvar ,@branches))))))

         ;; Assignment
         ((eq? tag '%setval!)
          (apply (op 'op-assign) (map1 name->index args)))
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
                (body (cdr args)))
            (let scan ((f formals)
                       (rnamed '()))
              (if (pair? f)
                  (scan (cdr f)
                        (cons (car f) rnamed))
                  (let ((named (reverse rnamed))
                        (rest (if (null? f) '() (list f))))
                    ((op 'op-lambda) (comp/e `(begin ,@body)
                                           (append named rest env))
                     ;; LSB = have-rest-arg
                     (+ (length rest)
                        (* 2 (length named)))))))))
         
         
         ;; Literal values
         ((eq? tag 'quote)
          ((op 'op-lit) (car args)))

         ;; Variable definition
         ((eq? tag 'let) ;; allow macro override
          (comp `(%let ,@args)))
         ((eq? tag '%let)
          (let ((bindings (car args))
                (body (cdr args)))
            (if (null? bindings)
                (comp `(begin ,@body))
                (let ((names (map1 car bindings))
                      (exprs (map1 cadr bindings)))
                  ((op 'op-let)
                   (reverse (map1 comp exprs))
                   (comp/e `(begin ,@body)
                           (append names env)))))))

         ;; Sequencing
         ((eq? tag '%seq)
          (apply (op 'op-seq) (map1 comp args)))
         ((eq? tag 'begin)
          (cond
           ((null? args) (comp '(void)))
           ((null? (cdr args)) (comp (car args)))
           (else (comp `(%seq ,(car args) (begin ,@(cdr args)))))))

         ;; Application: perform evaluation using a `let' form
         ;; and pass the result to `%app' which expects values.
         ((eq? tag '%app)
          (let ((ids (map1 name->index (car args))))
            ((op 'op-app) (car ids) (list->vector (cdr ids)))))
         (else
          (let ((vars (map1 memoize-var form)))
            (comp `(let ,(memo-bindings vars form) (%app ,vars)))))
         )))
         
     ;; Constant
     (else
      (comp (list 'quote form)))))

  ;; Compile and gather free variables.
  (let ((code (compile form '())))
    (list code free))))


;; Compiler macro dependencies.
(define vm-macros
  (list (cons 'quasiquote expand-quasiquote)
        (cons 'cond       expand-cond)
        (cons 'and        expand-and)
        (cons 'or         expand-or)
        (cons 'let        expand-let)
        (cons 'letrec     expand-letrec)
        (cons 'lambda     (lambda (e) ;; FIXME: cold VM compat
                            (expand-lambda e (lambda (x) x))))))

;; Initial toplevel.
(define vm-toplevel
  (append
   (filter (lambda (x) (prim? (cdr x))) (toplevel))
   `((+             . ,add)
     (*             . ,mul))))

(define (vm-compile expr)
  (vm-compile/macros expr
                     vm-toplevel
                     vm-macros))

(define (vm-run bytecode)
  (vm-init (vm-compile-anf bytecode))
  (vm-continue))

(define (vm-eval expr)
  (vm-run (car (vm-compile expr))))    ;; sexpr -> anf

