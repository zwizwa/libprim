#lang scheme/base

;; Bootstrap primitive init from C file.

;; (define re-def (regexp "_.\\s+?sc_\\s+?\\(.*?\\)"))

(define re-def (pregexp "_\\s+?sc_\\S*?\\(sc\\s+?\\*sc.*?\\)"))
(define (next)
  (let* ((line (read-line)))
    (when (not (eof-object? line))
      (let ((match (regexp-match re-def line)))
        (when match
          (write match)
          (newline))
        (next)))))

(next)

  
  