;; Utilities for Scheme<->Java interaction.

;; Convenient syntax for calls to the libprim.reflect static methods.
(define-macro (java form)
  `(java-call
    (vector ,(symbol->string (cadr form))
            (vector ,@(cddr form)))))

