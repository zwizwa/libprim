#lang scheme/base
(require scheme/pretty
         scheme/match)


;; Tools
(define (parse-formals formals)
  (let parse ((named '())
              (rest formals))
    (if (pair? rest)
        (parse (cons (car rest) named) (cdr rest))
        (values named rest))))

(define ((make-variable-lookup env) v)
  (let scan ((e env)
             (n 0))
    (cond
     ((null? e)
      ;; (raise 'undefined-variable v) ;; FIXME: module-level
      v
      )
     ((eq? v (car e))
      n)
     (else
      (scan (cdr e) (add1 n))))))

(define (make-name n)
  (let* ((new-name n)
         (sym (string->symbol (format "V~s" n))))
    (values sym (add1 n))))


;; Compile VM forms expressed as Scheme s-expression form to VM opcode
;; s-expression form.  Note that this performs only variable ->
;; debruyn conversion and not conversion to SSA form.
(define (comp-vm expr)
  (let ((OP_HALT   0)
        (OP_CALL   1)
        (OP_CLOSE  2)
        (OP_DROP   4)
        (OP_QUOTE  5)
        (OP_APP    7)
        (OP_IF     8)
        (OP_REF   11)
        (OP_DUMP  12)
        )

 
  (let comp ((expr expr)
             (env  '()))
    (define variable (make-variable-lookup env))
      (match expr

             ;; Bind result of intermediate evaluation to variable and
             ;; evaluate body in extended environment.  During
             ;; compilation we only need to keep track of names.
             ((list 'let (list (list var inter)) body)
              (cons OP_CALL
                    (cons
                     ;; The `body' expression will be evaluated in the
                     ;; extended environment.
                     (comp body (cons var env))
                     (comp inter env))))
             ;; Condition is varref.
             ((list 'if var yes no)
              (list OP_IF
                    (variable var)
                    (comp yes env)
                    (comp no env)))

             
             ;; Similar to `let' but ignore the return value.  This
             ;; uses the OP_CALL in followed by OP_DROP.
             ((list 'begin now later)
              (cons
               ;; This will cause the result of evaluation of the
               ;; `now' expression to be pushed to the top of the
               ;; environment stack.
               OP_CALL
               (cons (cons
                      ;; This removes the pushed return value before
                      ;; evaluating the other code.  Note that we
                      ;; don't extend the environment here, as opposed
                      ;; to the implementation of `let'.
                      OP_DROP
                      (comp later env))
                     (comp now env)
                     )))

             
             ;; Parse argument list, add the variable names to the
             ;; environment list passed down, add the number of
             ;; named+rest args (encoded in 1 number) to the closure
             ;; building op 13.
             ((list 'lambda formals expr)
              (let-values (((named rest) (parse-formals formals)))
                (cons OP_CLOSE
                      (cons (+ (length rest)
                               (* 2 (length named)))
                            (comp expr (append formals rest env))))))
             
             ;; Simpler special forms.
             ((list 'halt)         OP_HALT)
             ((list 'quote atom)  (list OP_QUOTE atom))
             ((list 'dump var)    (list OP_DUMP (variable var)))

             ;; Application
             ((list-rest fn args)
              (cons OP_APP (for/list ((e (cons fn args)))
                             (comp e env))))

             ;; Atoms
             (else
              (cond
               ;; Symbols are varrefs.
               ((symbol? expr)
                (cons OP_REF (variable expr)))
               ;; Other atoms are quotes.
               (else
                (comp (list 'quote expr) env))))))))





;; Compile Scheme expression form to the ANF dialect of the VM in
;; Scheme expression form.
(define (comp-anf expr)

  ;; Keep a global name generator state per expression.  Note that we
  ;; don't really need unique names for all generated variables - just
  ;; uniqueness for each lexical leg.  However, it's simpler to do it
  ;; globally.
  (define *name-state* 0)
    (define (name!)
      (let-values (((v n+) (make-name *name-state*)))
        (set! *name-state* n+)
        v))
  
  (let expand ((e expr))
    (match e
           ;; Conditional
           ((list 'if cond yes no)
            (let ((v (name!)))
              `(let ((,v ,cond))
                 (if ,v
                     ,(expand yes)
                     ,(expand no)))))
           ;; Unpacking begin
           ((list 'begin e) e)
           ((list-rest 'begin e es)
            `(begin ,(expand e)
                    ,(expand `(begin ,@es))))
           
           ;; Unpacking let*
           ((list 'let* '() form) form)
           ((list 'let* (list-rest b bs) form)
            `(let (,b) ,(expand `(let* ,bs ,form))))
            
           ;; Application -> let*
           ((list-rest fn args)
            (let* ((es (cons fn args))
                   (vs (for/list ((e es))
                         (if (symbol? e) e (name!)))))
              (expand 
               `(let* ,(for/list ((v vs)
                                  (e es)
                                  #:when (not (symbol? e)))
                         (list v e))
                  ,vs))))

           ;; Proper (parallel) let form.  Maybe this needs to go in
           ;; the comp-vm function.

           ;; Ignore others.
           (else
            e))))
  
(require scheme/pretty)
(define-syntax-rule (anf e)
  (pretty-print (comp-anf 'e)))



(define (compile-verbose exprs)
  (for ((expr exprs))
    (printf ";; ~a\n" expr)
    (pretty-print (comp-vm expr))))


;; These generate the VM test suite.  Each expression is a convoluted
;; way to produce the value 123.
(compile-verbose
 '('123
   (begin '321 '123)
   (let ((v '123)) v)
   
   (let ((v '1))  (if v '123 '234))
   (let ((v '#f)) (if v '234 '123))
   
   ; (begin (let ((v '(1 . 2))) (dump v)) 123)
   (let ((fn (lambda () 123))) (fn))

   (let ((v 123))
   (let ((fn (lambda () v)))
     (fn)))

   (let ((v 123))
   (let ((dummy 321))
   (let ((fn (lambda () v)))
     (fn))))
   
   (let ((v 123))
   (let ((d1 111))
   (let ((d2 222))
     v)))
   
   (let ((v 123))
   (let ((dummy 234))
   (let ((fn (lambda () v)))
     v)))
   
   ))

;; Extra tests
;(compile-verbose
; '((halt)
;   (lambda () 123)
;   ))

           
       