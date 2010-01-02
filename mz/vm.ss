




(define (vm-next c e k)

  ;; Continuations
  (define (k-if v k yes no)
    (if v
        (vm-next yes e k)
        (vm-next no  e k)))

  ;; Primitive instructions
  (define (c-ref n)
    (let ref ((e e) (n n))
      (if (zero? n)
          (car e)
          (ref (cdr e) (sub1 n)))))
  (define (c-value v)
    (let ((top (car k))
          (k   (cdr k)))
      (top v k)))
  
  (define (c-if cond yes no)
    (vm cond e
        (cons (lambda (v)
                (k-if v k yes no)))))

  
  (let ((opcode (car c))
        (args (cdr c)))
    (apply opcode args)))

   
           
    
        

(define (next c k)
  ;; c = code label
  ;; k = continuation (list of k-frames)
  (values c k))

(define (continuations . forms)
  (begin . forms))

(continuations
 ;; Primitive functions associated to continuation frames.  These take
 ;; a value and jump to the next machine state.  These can be
 ;; translated directly to EX code.
     
 (define (k-if c e k yes no)
   (if c ;; term is a value
       (next yes k)
       (next no  k)))

 (define (k-seq c e k term)
   (next term k))

 (define (k-apply value k todo done)
   (if (null? todo)
       
 
  