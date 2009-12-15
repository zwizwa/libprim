
;;; JAVA JNI + REFLECT TEST + DOC

(load "java.scm")
;; See here for the 'j' macro: it is used to call static methods
;; (functions) in the reflect.java class.  The rest of the file is
;; standard Scheme with calls into Java through this macro.

(define (do-post expr val)
  (write expr)
  (display " -> ")
  (write val)
  (newline)
  val)
(define-macro (post form)
  (let ((expr (cadr form)))
  `(do-post ',expr ,expr)))

  
;; Class & primitive type lookup
(define tests.type  (j type "zwizwa.libprim.tests")) 
(define int.type    (j type "int"))
(define string.type (j type "java.lang.String"))

;; Constructor lookup
;;                          <class>    <argtypes>
(define ctor (j constructor tests.type (vector string.type)))

;; Constructor invocation
;;                  <constructor> <args>
(define y (j create ctor          #("blah")))


;; Method lookup:       <class>    <name>  <argtypes>
(define foo   (j lookup tests.type "foo"   #()))      ;; static
(define make  (j lookup tests.type "make"  #()))      ;; static
(define state (j lookup tests.type "state" #()))
;; Same for class (static) and object methods.

;; Static method invocation
;;                  <object> <method> <args>
(post     (j invoke (void)   foo      #()))
(define x
  (post   (j invoke (void)   make     #()))) ;; a static factory method
(post     (j invoke x        state    #()))
(post     (j invoke y        state    #()))

;; Static method lookup with primitive types
(define addint (j lookup tests.type "addint" (vector int.type int.type)))

;; Primitive type invocation + Number Java->Scheme conversion
(post 
 (j invoke (void) addint
    (vector (j Integer "1")
            (j Integer "2"))))

'OK
