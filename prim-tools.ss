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

(define (_table-defs init ds prefix)
  (printf "#define ~a {\\\n" init)
  (for ((d ds))
    (let ((n (sub1 (length (regexp-split #rx"," d))))
          (name (car (regexp-match (re-name) d))))
      (printf "~a{~s, ~a, ~s},\\\n" prefix (mangle name) name n)))
  (printf "~a{}}\n" prefix))

(define-syntax-rule (for-each* fn lst)
  (for-each (lambda (l) (apply fn l)) lst))

(define (declarations->dict ds)
  (for/list ((d ds))
    (let ((n (sub1 (length (regexp-split #rx"," d))))
          (name (car (regexp-match (re-name) d))))
      (list name n))))

(define (table-defs init dict prefix)
  (printf "#define ~a {\\\n" init)
  (for-each*
   (lambda (name n)
     (printf "~a{~s, ~a, ~s},\\\n" prefix (mangle name) name n))
   dict)
  (printf "~a{}}\n" prefix))
     
(define (filename->initname f)
  (format "~a_init" (car (regexp-split #rx"\\." f))))

(define (gen)
  (let ((filename (arg0)))
    (gen-header
     (filename->initname filename)
     (scan filename))))

(define (gen-header init ds)
  (let ((dict (declarations->dict ds)))
    (decls ds)
    (table-defs init dict "")))

