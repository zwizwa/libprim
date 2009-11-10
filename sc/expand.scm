
;; Pre-expand the bootfile for faster load.
(define expanded
  (let ((flat
         (let ((exp (read (open-input-file "boot.scm"))))
           (let ((head (reverse (cdr (reverse exp))))
                 (tail (cadr (cadr (assq 'eval-list (cdr exp))))))
             (append head tail)))))
    (expand flat)))

(write expanded (open-output-file "boot-expanded.scm"))

  