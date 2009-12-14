;; Utilities for Scheme<->Java interaction.

;; The `j' form is a convenient syntax for calls to the
;; zwizwa.libprim.reflect static methods defined in reflect.java

;; Usage:  (j <method-name> <arg> ...)

(define-macro (j form)
  `(java-call
    (vector ,(symbol->string (cadr form))
            (vector ,@(cddr form)))))

;; Look up a method ID based on object, method name and method type
;; signature expressed as strings.
(define (jmethod obj name . args)
  (j method (j typeof obj) name
     (list->vector
      (map (lambda (typename) (j type typename)) args))))

;; Simpler syntax for method invocation
(define (jinvoke obj method . args)
  (j invoke obj method (list->vector args)))

;; Jim, your friendly all-in one method lookup and invocation hof.
(define (jim obj . types)
  (let ((m (apply jmethod obj types))) ;; resolve once
    (lambda args (apply jinvoke obj m args))))


(define unj java-unpack)


(define String (j type "java.lang.String"))