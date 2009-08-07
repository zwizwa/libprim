#lang scheme/base

;; Bootstrap primitive init from C file.


(define re-def (pregexp "_\\s+?sc_\\S*?\\(sc\\s*?\\*.*?\\)"))
(define re-name (pregexp "sc_\\S*?(?=\\()"))


(define (_mangle x) x)

(define (join lst)
  (apply string-append
         (cdr
          (apply append
                 (for/list ((l lst)) (list "-" l))))))

(define (mangle x)
  (let ((seg (cdr (regexp-split #rx"_" x))))
    (cond
     ((equal? "is" (car seg))
      (append (string-append (join (cdr seg)) "?")))
     (else (join seg)))))

;; (define (decls)
;;   (define lst '())
;;   (let* ((line (read-line)))
;;     (when (not (eof-object? line))
;;       (let ((match (regexp-match re-def line)))
;;         (when match
;;           (let ((str (car match)))
;;             (set! lst (cons str lst))))))))

(define (next)
  (let* ((line (read-line)))
    (when (not (eof-object? line))
      (let ((match (regexp-match re-def line)))
        (when match
          (let* ((str (car match))
                 (n (sub1 (length (regexp-split #rx"," str))))
                 (name (car (regexp-match re-name str))))
            (printf "~a; // DEF(~s, ~a, ~s);\n" str (mangle name) name n))
          
          )
        (next)))))

(display "typedef object _;\n")
(next)

  
  