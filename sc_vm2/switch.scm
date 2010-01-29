;; Simple dispatch form.

;; A Scheme compiler doesn't really need a full-blown decision tree
;; pattern match compiler.  A simple s-expression based dispatch
;; statement that uses lambda for list deconstruction and dispatches
;; on the first list element (symbol) is already quite powerful.


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

