;; Utilities for Scheme<->Java interaction.

;; The `j' form is a convenient syntax for calls to the
;; zwizwa.libprim.reflect static methods defined in reflect.java

;; Usage:  (j <method-name> <arg> ...)

(define-macro (j form)
  `(java-call
    (vector ,(symbol->string (cadr form))
            (vector ,@(cddr form)))))
