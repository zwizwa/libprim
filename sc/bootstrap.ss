#lang scheme/base
(require scheme/pretty)

;; Create bootstrap script for SC interpreter by glueing together
;; boot1.scm and boot2.scm scripts.  The latter is reused in the ANF
;; VM.

(define boot1 (read (open-input-file "boot1.scm")))
(define boot2 (for/list ((expr (in-port read (open-input-file "boot2.scm")))) expr))

(define boot
  (let find ((e boot1))
    (if (equal? e '(%boot2%)) boot2
        (if (pair? e)
            (cons (find (car e)) (find (cdr e)))
            e))))

(pretty-print boot)
