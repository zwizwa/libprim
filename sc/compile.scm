;; Preliminary expression compiler.


;; Compile expanded source code to AST (each expression is tagged).


(define (compile expr env)
  (define (frame lst)
    (let rec ((in lst)
              (out '()))
      (if (pair? in)
          (rec (cdr in) (cons (car in) out))
          (list (list->vector (reverse out))
                (and (not (null? in)) in)))))


  ;; Straightforward optimizations.
  (define (make-seq forms)
    (if (null? (cdr forms)) (car forms) `(%seq ,forms)))
  
  (let ((comp (lambda (expr) (compile expr env))))
    (cond
     ((pair? expr)
      (case (car expr)
        ((lambda)  (let*
                       ((args_opt (frame (cadr expr)))
                        (env (cons args_opt env)))
                     `(%lambda ,@args_opt
                               ,(make-seq (map (lambda (expr) (compile expr env))
                                               (cddr expr))))))
        ((set!)   `(%set! (%varbox ,(cadr expr)) ,(comp (caddr expr))))
        ((letcc)  `(%letcc, (cadr expr) ,(comp (caddr expr))))
        ((begin)   (make-seq (map comp (cdr expr))))
        ((quote)  `(%quote ,(cadr expr)))
        ((if)     `(%if ,@(map comp (cdr expr))))
        (else `(%app ,@(map comp (cdr expr))))))
     ((symbol? expr) `(%varref ,expr))
     (else `(%quote ,expr)))))
               

 