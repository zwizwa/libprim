#lang scheme/base

;; Bootstrap primitive init from C file.

;; (define re-def (regexp "_.\\s+?sc_\\s+?\\(.*?\\)"))

(define re-def (pregexp "_\\s+?sc_\\S*?\\(sc\\s*?\\*\\s*?sc.*?\\)"))
(define (next)
  (let* ((line (read-line)))
    (when (not (eof-object? line))
      (let ((match (regexp-match re-def line)))
        (when match
          (display (car match))
          (display ";\n"))
        (next)))))

(display "typedef object _;\n")
(next)

  
  