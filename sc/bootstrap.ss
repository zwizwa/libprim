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
           (`(%load ,filename) `(begin ,@(load->exprs filename)))
           ((a . d) (cons (find a) (find d)))
           (else e))))

(pretty-print
 (load-expand "boot1.scm"))

;; (pretty-print boot)
