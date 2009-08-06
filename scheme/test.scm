; (post ((lambda (abc) 123) 345))
; (((lambda (abc) (lambda (def) abc)) 123) 456)

; (post (cons 111 ((lambda (abc) 123) 456)))

((lambda (abc) 123) 456)
((lambda (ding) 
   (lambda (foo bar) ding))
 123)
  
