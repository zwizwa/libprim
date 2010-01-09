;; Support internal definitions
(define (expand-lambda lambda-form)
  (let ((define? (lambda (x)
                   (if (pair? x)
                   (if (pair? (car x))
                   (eq? 'define (caar x))
                   #f) #f))))
    (let collect ((body (cddr lambda-form))
                  (defs '()))
      (if (define? body)
          (collect (cdr body)
                   (cons (expand-define (car body)) defs))
          (let ((defs (reverse defs)))
            (list* '%lambda
                   (cadr lambda-form)
                   (if (null? defs)
                       body
                       (list (list* 'letrec (reverse defs) body)))))))))
