;; Utilities for Scheme<->Java interaction.



;; Interface to other zwizwa.libpim.* classes

;; Java classes
(define eval-class (java-class "zwizwa/libprim/eval"))
(define reflect-class (java-class "zwizwa/libprim/reflect"))

;; (java-methodID reflect-class "test1" "(II)I")

(define dumpMethods
  (let* ((sig "(Ljava/lang/Class;)V")
         (ID (java-static-methodID reflect-class "dumpMethods" sig)))
    ;; (begin (display "dumpMethods-ID: ") (write ID) (newline))
    (lambda (name)
      (java-call reflect-class ID (vector name) sig))))









;;; TEST



(define dummy-class (java-class "dummy"))

(define dummy-ctor-1 (java-methodID dummy-class "<init>" "()V"))
(define dummy-1 (java-new dummy-class dummy-ctor-1 #() "()V"))

(define dummy-ctor-2 (java-methodID dummy-class "<init>" "(Ljava/lang/String;)V"))
(define dummy-2 (java-new dummy-class dummy-ctor-2 (vector (java-string "foo")) "(Ljava/lang/String;)V"))

(define dummy-foo (java-static-methodID dummy-class "foo" "()V"))

(define dummy-str (java-static-methodID dummy-class "str" "(Ljava/lang/String;)V"))
(java-static-call dummy-class dummy-str (vector (java-string "asdf"))  "(Ljava/lang/String;)V")


