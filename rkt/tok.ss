#lang scheme/base
;;; Routines for the flat tokenized data structure representation.

;; Polish polish notation tokenized.  The tokenizer is a stream of
;; byte chunks, where each chunk is either quoted or not.  This is
;; enough to encode ADTs when the non-quoted chunks are mapped to
;; constructors.

(define emitter (make-parameter display))
(define (emit x) ((emitter) x))


(define (number->nibbles n)
  (let next ((n n)
            (lst '()))
    (if (zero? n) lst
        (next (arithmetic-shift n -4)
              (cons (bitwise-and n #xF) lst)))))

;; Low nibble is hex digit payload, high nibble is tag
(define tag-leading       #x20)
(define tag-trailing-code #x30)
(define tag-trailing-data #x40)

(define (encode-number size quoted)
  (let next ((in (number->nibbles size))
             (out '()))
    (let ((head (car in))
          (tail (cdr in)))
      (let ((out+ (cons
                   (+ head
                      (if (null? tail)
                          (if quoted
                              tag-trailing-data
                              tag-trailing-code)
                          tag-leading))
                   out)))
        (if (null? tail)
            (apply bytes (reverse out+))
            (next tail out+))))))

(define (symbol->bytes sym)
  (string->bytes/utf-8 (symbol->string sym)))

(define (emit-data bytes [quoted #t])
  (emit (encode-number (bytes-length bytes) quoted))
  (emit bytes))
(define (emit-code sym)
  (emit-data (symbol->bytes sym) #f))

;; Serializer for scheme data types
(define (unparse x)
  (define (ctr tag . args)
    (emit-code tag)
    (for ((a args)) (unparse a)))
  (define (prim tag bytes)
    (emit-code tag)
    (emit-data bytes))
  (cond
   ((null? x)   (ctr  'NIL))
   ((pair? x)   (ctr  'CONS (car x) (cdr x)))
   ((symbol? x) (prim 'SYMBOL (symbol->bytes x)))))


(define (test)
  (unparse '(foo bar (baz))))
