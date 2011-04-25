#lang scheme/base
(require scheme/pretty
         scheme/match)

(define (variable-index v env)
  (let scan ((e env)
             (n 0))
    (cond
     ((null? e)
      (raise 'undefined-variable v))
     ((eq? v (car e))
      n)
     (scan (cdr e) (add1 n)))))

(define (parse-formals formals)
  (let parse ((named '())
              (rest formals))
    (if (pair? rest)
        (parse (cons (car rest) named) (cdr rest))
        (values named rest))))

;; Compile base forms in Scheme s-expression form to VM opcode
;; s-expression form.
(define (comp expr)
  (let ((OP_HALT   0)
        (OP_LET    1)
        (OP_BEGIN  3)
        (OP_QUOTE  5)
        (OP_APP    7)
        (OP_IF     8)
        (OP_REF   11)
        (OP_CLOSE 13))

    (let _ ((expr expr)
            (env  '()))
      (match expr
             ((list 'halt)
              OP_HALT)
             ((list 'quote atom)
              (cons OP_QUOTE atom))
             ;; Like let, but doesn't bind value.
             ((list 'begin now later)
              (cons OP_BEGIN
                    (cons (_ now env)
                          (_ later env))))
             ;; Bind result of intermediate evaluation to variable and
             ;; evaluate body in extended environment.  During
             ;; compilation we only need to keep track of names.
             ((list 'let var inter body)
              (cons OP_LET
                    (cons (_ body (cons var env))
                          (_ inter env))))
             ;; Condition is varref.
             ((list 'if var yes no)
              (cons OP_IF
                    (cons (variable-index var env)
                          (cons (_ yes env)
                                (_ no env)))))
             ;; Parse argument list, add the variable names to the
             ;; environment list passed down, add the number of
             ;; named+rest args (encoded in 1 number) to the closure
             ;; building op 13.
             ((list 'lambda formals expr)
              (let-values (((named rest) (parse-formals formals)))
                (cons OP_CLOSE
                      (cons (+ (length rest)
                               (* 2 (length named)))
                            (_ expr (append formals rest env))))))
             ;; Application

             ;; FIXME: looks like there's a bug in the VM: fn and args
             ;; all need to be varrefs, not in-line forms.
             
             ;; ((list-rest fn args)  (cons OP_APP (_ fn  ...)))
             
             
             (else
              (cond
               ;; Symbols are varrefs.
               ((symbol? expr)
                (cons OP_REF (variable-index expr env)))
               ;; Other atoms are quotes.
               (else
                (_ (list 'quote expr) env))))))))


(for ((expr
       '((halt)
         (lambda () 123)
         ;; Original test suite
         '123
         (begin '321 '123)
         (let v '123 v)
         (let v '1 (if v '123 '234))
         )))
  (pretty-print (comp expr)))
      


           
       