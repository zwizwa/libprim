(take-vector 3 '(a b c d))
(take-vector 3 '(a b))

(length '(a b))
(length 123)
(list->vector '(1 2 3))
(make-vector 10 123)

;; Bootstrap
 



;; (begin)
(if (zero? 1) 2)
(post 'before-defmacro)
(push-toplevel-macro 'testmac
                     (lambda (form)
                       (post form)
                       '(post 'ok)))
(post 'before-macro)
(testmac)
((lambda ()
   (post 'a)
   (post 'b)
   123))
;; (begin
;;   (post 'foo)
;;   (post 'bar)
;;   123)
;; (if (zero? 1) 123 345)
;; (setvar 'foo 456)
;; foo
;; (set! foo 123)
;; foo
;; (gc)
;; (setvar 'foo
;;         (lambda (lst _)
;;           (foo (cdr lst) (post 'foo-foo))))
;; (foo '(_ _ _ _ _ _ _) '_)

;; (begin a b c)

;; ((lambda (_ _ rv) rv) a b c)

;; (post 'fffffffffffffffffffff)




;; (gc)
;; (((lambda (abc) (lambda (def) abc)) 123) 456)

;; (lambda (bie) bie)

;; (post ((lambda (abc) 123) 345))

;; (post (cons 111 ((lambda (abc) 123) 456)))

;; (post
;;  ((lambda (ding) 
;;     (lambda (foo bar) ding))
;;   123))
  
