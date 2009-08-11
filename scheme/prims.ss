#lang scheme/base

;; Bootstrap primitive init from C file.

(define re-def (pregexp "_\\s+?sc_\\S*?\\(sc\\s*?\\*.*?\\)"))
(define re-name (pregexp "sc_\\S*?(?=\\()"))

(define (pre->suf pre x suf)
  (if (regexp-match pre x)
      (string-append (regexp-replace pre x "") suf)
      x))

(define (mangle x)
  (let* ((x (regexp-replace  #px"^sc_"   x ""))
         (x (regexp-replace* #px"_"      x "-"))
         (x (regexp-replace* #px"-to-"   x "->"))
         (x (pre->suf        #px"^bang-" x "!"))
         (x (pre->suf        #px"^is-"   x "?"))
         )x))

(define (scan)
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

(define (defs ds prefix)
  (for ((d ds))
    (let ((n (sub1 (length (regexp-split #rx"," d))))
          (name (car (regexp-match re-name d))))
      (printf "~aDEF(~s, ~a, ~s);\n" prefix (mangle name) name n))))

(define (table-defs ds prefix)
  (printf "#define prims_init {\\\n")
  (for ((d ds))
    (let ((n (sub1 (length (regexp-split #rx"," d))))
          (name (car (regexp-match re-name d))))
      (printf "~a{~s, ~a, ~s},\\\n" prefix (mangle name) name n)))
  (printf "~a{}}\n" prefix))
  
(define (gen-header ds)
  (display "#include \"scheme.h\"\n")
  (decls ds)
  (table-defs ds "")
;  (printf  "static inline void _sc_def_prims(sc *sc){\n")
;  (defs ds "    ")
;  (printf  "}\n")
  )

(gen-header (scan))
