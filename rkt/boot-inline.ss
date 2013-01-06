#lang scheme/base
(require scheme/pretty
         mzlib/match)

;; Create bootstrap script for SC interpreter by expanding loaded
;; files in-line.

(define (load->exprs filename)
  (with-input-from-file filename
    (lambda ()
      (for/list ((e (in-port))) e))))

(define (load-expand filename)
  (let find ((e (car (load->exprs filename))))
    (match e
           (`((%load ,filename) . ,d)
            (append (load->exprs filename) (find d)))
           ((a . d)
            (cons (find a) (find d)))
           (else e))))

(pretty-print
 (load-expand
  (vector-ref (current-command-line-arguments) 0)))
   


;; (pretty-print boot)
