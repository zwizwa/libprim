;; Utilities for Scheme<->Java interaction.

;; Convenient syntax for calls to the libprim.reflect static methods.
(define-macro (java form)
  `(java-call
    (vector ,(symbol->string (cadr form))
            (vector ,@(cddr form)))))


;;; TEST

(define tests (java type "zwizwa.libprim.tests"))
(define foo   (java lookup tests "foo" #()))
(define make  (java lookup tests "make" #()))

(java invoke (void) foo #())
(define x (java invoke (void) make #())) ;; a static factory method

(define state (java lookup tests "state" #()))
(java invoke x state #())

(define ctor  (java constructor tests (vector (java type "java.lang.String"))))
(java create ctor #("blah"))
