#lang scheme/base

(require "tools.ss")
(provide (all-defined-out))

(define re-def (make-parameter #f))
(define re-name (make-parameter #f))
(define ctx (make-parameter #f))
(define macro-prefix (make-parameter ""))

(define (pre->suf pre x __suf)
  (if (regexp-match pre x)
      (string-append (regexp-replace pre x "") __suf)
      x))
(define (pre->pre pre x pre__)
  (if (regexp-match pre x)
      (string-append pre__ (regexp-replace pre x ""))
      x))

;; Order is important (for combinations)
(define (mangle x)
  (let* ((x (regexp-replace  #px"^\\S*?_" x ""))
         (x (regexp-replace* #px"_"       x "-"))
         (x (regexp-replace* #px"-to-"    x "->"))
         (x (regexp-replace* #px"-with-"  x "/"))
         (x (pre->suf        #px"^bang-"  x "!"))
         (x (pre->suf        #px"^fetch-" x "@"))
         (x (pre->suf        #px"^from-"  x ">"))
         (x (pre->suf        #px"^is-"    x "?"))
         (x (pre->pre        #px"^to-"    x ">"))
         )x))

(define (macro-mangle x)
  (let* ((x (regexp-replace  #px"^\\S*?_"   x "")))
    (string-upcase x)))

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


(define (mprefix str)
  (string-append (macro-prefix) str))

(define (macro-defs dict ctx)
  (for-each*
   (lambda (name n)
     (let ((mname (mprefix (macro-mangle name))))
       ;; (printf "#ifndef ~a\n" mname) ;; (*)
       (printf "#define ~a(~a) ~a(~a)\n"
               mname
               (string-append* (list->args (n->args n)))
               name
               (string-append* (list->args (cons ctx (n->args n)))))
       ;; (printf "#endif\n")
       ))
   dict))

;; This (*) is really asking for trouble.  Just make sure it doesn't
;; happen.

(define (filename->name f postfix)
  (format "~a~a" (car (regexp-split #rx"\\." f)) postfix))

(define (gen)
  (let ((file-path (arg0)))
    (let-values (((dir filename-p _) (split-path file-path)))
      (let ((filename (path->string filename-p)))
        ;; (printf "F:~a\n" filename)
        (gen-header
         (mprefix (filename->name filename "_table_init"))
         (mprefix (string-upcase (filename->name filename "")))
         (scan file-path)
         (ctx))))))

(define (gen-header init guard ds ctx)
  (let ((dict (declarations->dict ds)))
    (printf "#ifndef _~a_H_GEN_\n" guard)
    (printf "#define _~a_H_GEN_\n" guard)
    (decls ds)
    (table-defs init dict "")
    (macro-defs dict ctx)
    (printf "#endif\n")
    ))


;; Call this with the prefix tag.  See sc/sc_prims.ss
(define (ex-gen tag [ctx-tag tag])
  (parameterize
      ((re-def (pregexp (string-append "_\\s+?" tag "_\\S*?\\(" tag "\\s*?\\*.*?\\)")))
       (re-name (pregexp (string-append tag "_\\S*?(?=\\()")))
       (ctx ctx-tag))
    (gen)))
