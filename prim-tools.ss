#lang scheme/base

(provide (all-defined-out))

(define re-def (make-parameter #f))
(define re-name (make-parameter #f))

(define (pre->suf pre x suf)
  (if (regexp-match pre x)
      (string-append (regexp-replace pre x "") suf)
      x))

(define (mangle x)
  (let* ((x (regexp-replace  #px"^\\S*?_"   x ""))
         (x (regexp-replace* #px"_"      x "-"))
         (x (regexp-replace* #px"-to-"   x "->"))
         (x (pre->suf        #px"^bang-" x "!"))
         (x (pre->suf        #px"^is-"   x "?"))
         )x))


(define (arg0) (vector-ref (current-command-line-arguments) 0))

(define (scan filename)
  (with-input-from-file filename
    (lambda ()
      (define lst '())
      (let next ()
        (let* ((line (read-line)))
          (when (not (eof-object? line))
            (let ((match (regexp-match (re-def) line)))
              (when match
                (let ((str (car match)))
                  (set! lst (cons str lst))))
              (next)))))
      lst)))

(define (decls ds)
  (for ((d ds))
    (printf "~a;\n" d)))

(define (defs ds prefix)
  (for ((d ds))
    (let ((n (sub1 (length (regexp-split #rx"," d))))
          (name (car (regexp-match (re-name) d))))
      (printf "~aDEF(~s, ~a, ~s);\n" prefix (mangle name) name n))))

(define (table-defs init ds prefix)
  (printf "#define ~a {\\\n" init)
  (for ((d ds))
    (let ((n (sub1 (length (regexp-split #rx"," d))))
          (name (car (regexp-match (re-name) d))))
      (printf "~a{~s, ~a, ~s},\\\n" prefix (mangle name) name n)))
  (printf "~a{}}\n" prefix))

(define (filename->initname f)
  (format "~a_init" (car (regexp-split #rx"\\." f))))

(define (gen)
  (let ((filename (arg0)))
    (gen-header
     (filename->initname filename)
     (scan filename))))


(define (gen-header init ds)
;  (display "#include \"scheme.h\"\n")
  (decls ds)
  (table-defs init ds "")
;  (printf  "static inline void _sc_def_prims(sc *sc){\n")
;  (defs ds "    ")
;  (printf  "}\n")
  )

