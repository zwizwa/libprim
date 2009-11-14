;; Preliminary expression compiler.



;; (define (qq expr)
;;   (if (pair? expr)
;;       (let ((tag (car expr)))
;;         (cond
;;          ((eq? 'unquote tag) (cadr expr))
;;          ((list? expr)
;;           (
;;          (else
;;           (list 'cons (qq (car expr)) (qq (cdr expr))))))
;;       (list 'quote expr)))



;; (define (compile expr env)
;;   (cond
;;    ((pair? expr)
;;     (cond
;;      ()))
;;    ((symbol? expr)
;;     `(%ref ,expr))))
    