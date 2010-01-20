
(define (test-ref)
  (vm-init
   (vector (op-let1)
           (vector (op-lit) 123)
           (vector (op-ref) 0)))
  (vm-continue))

(define vm-thunk
  (vector (list "shamajee") ;; environment
          (vector (op-prim) (prim-fn write-stderr) #(0)) ;; body
          ))

(define (test-prim)
  (vm-init
   (vector (op-let1)
           (vector (op-lit) 123)
           (vector (op-prim) (prim-fn write-stderr) #(0))))
  (vm-continue))

(define (test-thunk t)
  (vm-init
   (vector (op-apply) t #()))
  (vm-continue))



                              