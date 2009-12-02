#lang scheme/base
(require xml scheme/pretty mzlib/match)

(define (read-xexpr . a)
  (xml->xexpr (document-element (apply read-xml a))))

(define (write-xexpr e . a)
  (apply display (xexpr->string e) a))

(define (xexpr->dict xexpr)
  (define (string-whitespace? str)         
    (let rest? ((l (string->list str)))
      (or (null? l)
          (and (char-whitespace? (car l))
               (rest? (cdr l))))))
    (define (raw-string str) (cons '$ str))
    (define (clean-element kar kdr)
      (if (string? kar)
          (if (string-whitespace? kar)
              kdr
              (cons (raw-string kar) kdr))
          (cons (xexpr->dict kar) kdr)))
    (define (clean-attribute a)
      (cons (car a)
            (cadr a)))
    (match xexpr
           ((tag attributes . elements)
            (case tag
              ((xhtml) xexpr) ;; preserve subtree
              (else           ;; convert to dictionary
               (cons tag
                     (append
                      (map clean-attribute
                           attributes)
                      (foldr clean-element
                             '() elements))))))))



(pretty-print (read-xexpr))