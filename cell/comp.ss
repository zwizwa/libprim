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
      

;; Compile base forms in Scheme s-expression form to VM opcode
;; s-expression form.
(define (comp expr)
  (let _ ((expr expr)
          (env  '()))
    (match expr
           ((list 'halt)
            0)
           ((list 'quote atom)
            (cons 5 atom))
           ;; Like let, but doesn't bind value.
           ((list 'begin now later)
            (cons 3
                  (cons (_ now env)
                        (_ later env))))
           ;; Bind result of intermediate evaluation to variable and
           ;; evaluate body in extended environment.  During
           ;; compilation we only need to keep track of names.
           ((list 'let var inter body)
            (cons 1
                  (cons (_ body (cons var env))
                        (_ inter env))))
           ;; Condition is varref.
           ((list 'if var yes no)
            (cons 8 (cons (variable-index var env)
                          (cons (_ yes env)
                                (_ no env)))))
           ;; ((list 'lambda formals expr) ...)
           ;; This inserts code to construct a closure.

           (else
            (cond
             ;; Symbols are varrefs.
             ((symbol? expr)
              (cons 11 (variable-index expr env)))
             ;; Handle unknown forms.
             ((pair? expr)
              (raise 'unknown-form expr))
             ;; Other atoms are quotes.
             (else
              (_ (list 'quote expr))))))))


(for ((expr
       '((halt)
         ;; Original test suite
         '123
         (begin '321 '123)
         (let v '123 v)
         (let v '1 (if v '123 '234))
         )))
  (pretty-print (comp expr)))
      


           
       