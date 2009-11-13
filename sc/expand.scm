
;; Pre-expand the bootfile for faster load.
(define flat
  (let ((exp (read (open-input-file "boot.scm"))))
    (let ((head (reverse (cdr (reverse exp))))
          (tail (cadr (cadr (assq 'eval-list (cdr exp))))))
      (append head tail))))

(define (save-expr file expr)
  (let ((port (open-output-file file)))
    (display "(" port)
    (for-each (lambda (expr) (write expr port) (newline port)) expr)
    (display ")\n" port)
    (close-port port)))

(save-expr "boot-expanded.scm" (expand flat))


