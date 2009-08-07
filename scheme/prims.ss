#lang scheme/base

;; Bootstrap primitive init from C file.


(define re-def (pregexp "_\\s+?sc_\\S*?\\(sc\\s*?\\*.*?\\)"))
(define re-name (pregexp "sc_\\S*?(?=\\()"))

(define (join lst)
  (apply string-append
         (cdr
          (apply append
                 (for/list ((l lst)) (list "-" l))))))

(define (mangle x)
  (define seg (cdr (regexp-split #rx"_" x)))
  (define (first? str) (equal? str (car seg)))
  (define (join/postfix str) (append (string-append (join (cdr seg)) str)))
  (let ((seg (cdr (regexp-split #rx"_" x))))
    (cond
     ((first? "is")   (join/postfix "?"))
     ((first? "bang") (join/postfix "!"))
     (else (join seg)))))

(define (collect)
  (define lst '())
  (let next ()
    (let* ((line (read-line)))
      (when (not (eof-object? line))
        (let ((match (regexp-match re-def line)))
          (when match
            (let ((str (car match)))
              (set! lst (cons str lst))))
          (next)))))
  lst)

(define *decls* (collect))

(define (decls)
  (display "typedef object _;\n")
  (for ((d *decls*))
    (printf "~a;\n" d)))

(define (defs [prefix "    "])
  (for ((d *decls*))
    (let ((n (sub1 (length (regexp-split #rx"," d))))
          (name (car (regexp-match re-name d))))
      (printf "~aDEF(~s, ~a, ~s);\n" prefix (mangle name) name n))))

;; (define (next)
;;   (let* ((line (read-line)))
;;     (when (not (eof-object? line))
;;       (let ((match (regexp-match re-def line)))
;;         (when match
;;           (let* ((str (car match))
;;                  (n (sub1 (length (regexp-split #rx"," str))))
;;                  (name (car (regexp-match re-name str))))
;;             (printf "~a; // DEF(~s, ~a, ~s);\n" str (mangle name) name n))
          
;;           )
;;         (next)))))

(decls)
(printf "static inline void _sc_def_prims(sc *sc){\n")
(defs)
(printf "}\n")
  
  