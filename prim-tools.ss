#lang scheme/base

(require "tools.ss"
         "sc/mangle.ss")
(provide (all-defined-out))

(define re-def (make-parameter #f))
(define re-name (make-parameter #f))
(define ctx (make-parameter #f))
(define macro-prefix (make-parameter ""))


;; Order is important (for combinations)

(define mangle c->scheme)

(define (macro-mangle x)
  (string-upcase (remove-ctx x)))

(define (remove-ctx x)
  (regexp-replace #px"^\\S*?_" x ""))


(define (arg n) (vector-ref (current-command-line-arguments) n))

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
  (let ((namespace (string-downcase ctx)))
    ;; Tag names with nb of arguments.  This allows non-tagged names
    ;; in C source code, but places tgged names in objects.
    ;; I.e. #define ex_foo ex_3_foo
    ;; (for-each*
    ;;  (lambda (name n)
    ;;    (printf "#define ~a ~a_~a_~a\n" name namespace n (remove-ctx name)))
    ;;  dict)
    ;; Create bound macros.  I.e. #define EX_FOO(a,b,c) ex_foo(EX, a, b, c)
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
     dict)))

;; This (*) is really asking for trouble.  Just make sure it doesn't
;; happen.

(define (filename->name f postfix)
  (format "~a~a" (car (regexp-split #rx"\\." f)) postfix))

(define (gen)
  (with-handlers ((void void))
    (delete-file (arg 1)))
  (let ((file-path (arg 0)))

    (let-values (((dir filename-p _) (split-path file-path)))
      (let ((filename (path->string filename-p)))
        ;; (printf "F:~a\n" filename)
        (parameterize ((current-output-port
                        (open-output-file (arg 1))))
          (gen-header
           (mprefix (filename->name filename "_table_init"))
           (mprefix (string-upcase (filename->name filename "")))
           (scan file-path)
           (ctx)))))))

(define (gen-header init guard ds ctx)
  (let ((dict (declarations->dict ds)))
    (printf "#ifndef _~a_H_GEN_\n" guard)
    (printf "#define _~a_H_GEN_\n" guard)
    (macro-defs dict ctx)
    (decls ds)
    (table-defs init dict "")
    (printf "#endif\n")
    ))


;; Call this with the prefix tag.  See sc/sc_prims.ss
(define (ex-gen tag [ctx-tag tag])
  (parameterize
      ((re-def (pregexp (string-append "_\\s+?" tag "_\\S*?\\(" tag "\\s*?\\*.*?\\)")))
       (re-name (pregexp (string-append tag "_\\S*?(?=\\()")))
       (ctx ctx-tag))
    (gen)))
