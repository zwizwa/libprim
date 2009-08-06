(setvar 'foo
        (lambda (_)
          (foo (post 'foo-foo))))
(foo 123)


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
  
