;; Simple dispatch form.

;; A Scheme compiler doesn't really need a full-blown decision tree
;; pattern match compiler.  A simple s-expression based dispatch
;; statement that uses lambda for list deconstruction and dispatches
;; on the first list element (symbol) is already quite powerful.


;; Generate names with gensym
(define-macro (let-names form)
  (let ((names (cadr form))
        (body  (cddr form)))
    `(let ,(map (lambda (name) `(,name (gensym))) names)
       ,@body)))

;; After this macros are simpler with defmacro, quasiquote and
;; let-names.
(define-macro (defmacro form)
  (let-names (frm)
    `(define-macro (,(cadr form) ,frm)
       (apply (lambda ,(caddr form) ,@(cdr (cddr form))) (cdr ,frm)))))



(defmacro switch (var . clauses)
  (let-names (tag args)
    `(let* ((,args ,var)
            (,tag  (car ,args))
            (,args (cdr ,args)))
       ,(let next ((cs clauses))
          (if (null? cs) '(void)
              (let ((c (car cs)))
                (if (eq? 'else (car c))
                    (cadr c)
                    `(if (eq? ,tag ',(car c))
                         (apply (lambda ,(cadr c) ,@(cddr c)) ,args)
                         ,(next (cdr cs))))))))))

