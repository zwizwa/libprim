;; Simple dispatch form.

;; A Scheme compiler doesn't really need a full-blown decision tree
;; pattern match compiler.  A simple s-expression based dispatch
;; statement that uses lambda for list deconstruction is already quite
;; powerful.

(define-macro (let-names form)
  (let ((names (cadr form))
        (body (cddr form)))
    
    `(let ,@(map (lambda (name) `(,name (gensym))) names)
       ,@body)))

(define-macro (switch form)
  (let ((var (cadr form))
        (form_ (gensym))
        (tag_  (gensym))
        (args_ (gensym))
        (clauses (cddr form)))
    `(let* ((,form_ ,var)
            (,tag_ (car ,form_))
            (,args_ (cdr ,form_)))
       ,(let next ((cs clauses))
          (if (null? cs) '(void)
              (let ((c (car cs)))
                (if (eq? 'else (car c))
                    (cadr c)
                    `(if (eq? ,tag_ ',(car c))
                         (apply (lambda ,(cadr c) ,@(cddr c)) ,args_)
                         ,(next (cdr cs))))))))))
                          