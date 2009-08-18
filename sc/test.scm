

;; (reverse! '(1 2 3))
(bool? 123)
(open-mode-file "/tmp/blubber" "w")
(string->symbol "asdfasFD")
(symbol->string 'asdfasdf)
(begin (make-vector 10000 123) 'foo)
(map2-prim cons '(a b c) '(1 2 3))


(list-clone '(a b c))
(list-clone '())
(list-clone '(a . b))
(list* 1 2 3)
(list 1 2 3)
(list* 1 2)

(define x (with-ck () 123)) x
(with-ck (cdr x) 124)
(with-ck (cdr x) 124)
(define x (with-ck (cdr x) 124)) x
(define x (with-ck (cdr x) 125)) x
(define x (with-ck (cdr x) 126)) x

;; (when 123 broebel)

(let ((broebel 123)) broebel)
(let foo ((n 10))
  (if (zero? n) 0
      (begin
        (post 'bla)
        (foo (sub1 n)))))


(post 'READ-TEST)
(post (read))




;; (letrec-tx '(letrec ((x 123)) foo))
;; (list* 1 2 '(a b c))
;; (length 123)
;; (car '())
;; (eq? 1 0)

;; (define lparen 40)
;; (unless 123 broebel)

;; (define rparen 41)

;; ;; (read-char)


;; ((make-definer 'def-toplevel-macro!) '(define (abc asdf) 123))
;; (eval '(post 123))
;; (gc-used)
;; (gc)
;; (gc-used)

;; (make-value 123)
;; (eval-step (make-state (make-redex 'a '((a . 123))) (mt)))



;; (bla)
;; (gc)
;; (apply post '(bar-bar))
;; (define foo
;;   (lambda ()
;;     (foo)))

;; (foo)

;; (begin
;;   (write (apply post '(foefel)))
;;   (letcc k (begin
;;              (post 'one)
;;              (k 123)
;;              (post 'two)))
;;   (post 'three)
       

;;   (define apply
;;     (lambda (fn args)
;;       (make-ast
;;        (cons fn
;;              (map (lambda (x)
;;                     (list 'quote x))
;;                   args)))))

;; )
;; (apply post '(bar-bar))

;; (make-ast '(post 'foo-foo))

;; (let ((foo 123)) foo)
;; (words)
;; (macros)




;; (define-syntax foo (lambda _ '(post 123)))
;; ;; (def-toplevel-macro! 'foo (lambda _ '(post 123)))
;; (foo)

;; (define foo 123)
;; foo

;; (list 1 2 3)

;; ((lambda (a . rest) rest) 123 1 2 3)

;; bork

;; ((lambda args args) 1 2 3)

;; (lambda (a) a)
;; (lambda (a . rest) rest)
;; (take-vector 3 '(a b c d))
;; (take-vector 3 '())
;; (take-vector 3 '(a b))

;; (length '(a b))
;; (length 123)
;; (list->vector '(1 2 3))
;; (make-vector 10 123)

;; ;; Bootstrap
 



;; ;; (begin)
;; (if (zero? 1) 2)
;; (post 'before-defmacro)
;; (push-toplevel-macro 'testmac
;;                      (lambda (form)
;;                        (post form)
;;                        '(post 'ok)))
;; (post 'before-macro)
;; (testmac)
;; ((lambda ()
;;    (post 'a)
;;    (post 'b)
;;    123))
;; ;; (begin
;; ;;   (post 'foo)
;; ;;   (post 'bar)
;; ;;   123)
;; ;; (if (zero? 1) 123 345)
;; ;; (setvar 'foo 456)
;; ;; foo
;; ;; (set! foo 123)
;; ;; foo
;; ;; (gc)
;; ;; (setvar 'foo
;; ;;         (lambda (lst _)
;; ;;           (foo (cdr lst) (post 'foo-foo))))
;; ;; (foo '(_ _ _ _ _ _ _) '_)

;; ;; (begin a b c)

;; ;; ((lambda (_ _ rv) rv) a b c)

;; ;; (post 'fffffffffffffffffffff)




;; ;; (gc)
;; ;; (((lambda (abc) (lambda (def) abc)) 123) 456)

;; ;; (lambda (bie) bie)

;; ;; (post ((lambda (abc) 123) 345))

;; ;; (post (cons 111 ((lambda (abc) 123) 456)))

;; ;; (post
;; ;;  ((lambda (ding) 
;; ;;     (lambda (foo bar) ding))
;; ;;   123))
  
