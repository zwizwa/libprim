(if (zero? 1) 123 345)
(setvar 'foo
        (lambda (lst _)
          (foo (cdr lst) (post 'foo-foo))))
(foo '(_ _ _ _ _ _ _) '_)

(begin a b c)

((lambda (_ _ rv) rv) a b c)

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
  
