#lang scheme/base
(require (planet dvanhorn/record-case:1:1/record-case))

(define (cps-exp body) body)

(define (cps-simple exp)
  (record-case exp
   (lambda (args body)
     `(lambda ,(append args '(k))
        ,(cps-exp body)))))
      

