(define A (make-grid-2 10 10 0.0))
(define V (make-grid-2 10 10 0.0))
(define S (make-grid-1 10 0.0))
(define work (make-grid-1 10 0.0))

(define (go)
  (grid-svd A V S work)
  (grid-dump A (current-output-port)))
