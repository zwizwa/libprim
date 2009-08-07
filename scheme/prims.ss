#lang scheme/base

;; Bootstrap primitive init from C file.

(define re-def (pregexp "_\\s+?sc_\\S*?\\(sc\\s*?\\*.*?\\)"))
(define re-name (pregexp "sc_\\S*?(?=\\()"))

(define (pre->suf pre x suf)
  (if (regexp-match pre x)
      (string-append (regexp-replace pre x "") suf)
      x))

(define (mangle x)
  (let* ((x (regexp-replace #rx"^sc_" x ""))
         (x (regexp-replace* #rx"_" x "-"))
         (x (regexp-replace* #rx"-to-" x "->"))
         (x (pre->suf #rx"^bang-" x "!"))
         (x (pre->suf #rx"^is-" x "?"))
         )x))
         


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

(define (decls ds)
  (display "typedef object _;\n")
  (for ((d ds))
    (printf "~a;\n" d)))

(define (defs ds [prefix "    "])
  (for ((d ds))
    (let ((n (sub1 (length (regexp-split #rx"," d))))
          (name (car (regexp-match re-name d))))
      (printf "~aDEF(~s, ~a, ~s);\n" prefix (mangle name) name n))))

(define (gen-header)
  (define ds (collect))
  (decls ds)
  (printf "static inline void _sc_def_prims(sc *sc){\n")
  (defs ds)
  (printf "}\n"))

(gen-header)
