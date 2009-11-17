(define A (make-grid-2 10 10 0.0))
(define V (make-grid-2 10 10 0.0))
(define S (make-grid-1 10 0.0))
(define work (make-grid-1 10 0.0))

(define (go)
  (grid-svd A V S work)
  (grid-dump A (current-output-port)))


(define (svd A)
  (let ((dims (grid-dims H)))
    (let ((N (vector-ref dims 0))
          (M (vector-ref dims 1)))
      (let ((U (grid-copy A))
            (V (make-grid-2 N N 0.0))
            (S (make-grid-1 N 0.0))
            (work (make-grid-1 N 0.0)))
        (grid-svd U V S work)
        (list U S V)))))

(define (svd-solve b U S V)
  (let* ((dims (grid-dims U))
         (N (vector-ref dims 0))
         (x (make-grid-1 N 0.0)))
    (grid-svd-solve U V S b x)
    x))

(define (mul-mv A x)
  (let ((y (make-grid-1 (vector-ref (grid-dims A) 1) 0.0)))
    (grid-mul-mv A x y) y))
    

;; TEST
(define H (grid-hankel (vector->grid #(1 2 3 4 5 6 7)) 3))
(define d (svd H))


(define A (make-grid-2 2 2 1.1))
(define x (make-grid-1 2 1.0))
(define (test A x)
  (let ((out (make-grid-2 2 10 0.0)))
    (grid-unfold A x out) out))



