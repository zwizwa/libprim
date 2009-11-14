;; Bootstrap

;; A single s-expression will be allocated outside the VM and passed
;; to the interpretation step.  This means there is no GC during
;; construction as long as it fits in the initial cell store: see
;; _sc_init() in scheme.c
(begin

;; Macro expander + support.
(def-toplevel! 'assq
  (lambda (obj lst)
    (if (null? lst) #f
        (if (eq? (caar lst) obj) (car lst)
            (assq obj (cdr lst))))))
(def-toplevel! 'map1
  (lambda (fn lst)
    (if (null? lst) lst
        (cons (fn (car lst))
              (map1 fn (cdr lst))))))
(def-toplevel! 'expand
  (lambda (expr)
    (if (pair? expr)
        ((lambda (tag)
           (if (eq? tag 'quote) expr
           (if (eq? tag 'lambda) (cons 'lambda (cons (cadr expr) (map1 expand (cddr expr))))
               ((lambda (rec)
                  (if rec
                      (expand ((cdr rec) expr))
                      (map1 expand expr)))
                (assq (car expr) (toplevel-macro))))))
         (car expr))
        expr)))
;; Implemented in terms of primitive continuation transformers (ktx).
(def-toplevel! 'eval
  (lambda (expr) (letcc k ((eval-ktx k (expand expr))))))

;; Evaluate expressions in sequence.  This makes sure macros take
;; effect immediately after definition.
(def-toplevel! 'eval-list
  (lambda (expr)
    (if (null? expr) (void)
        (begin (eval (car expr)) (eval-list (cdr expr))))))

;; The rest is evaluated in sequence with `eval' defined above, which
;; also performs macro expansion.
(eval-list '(

(def-toplevel! 'list (lambda args args))

(def-toplevel-macro!
  'define
  (lambda (form)
    (list 'def-toplevel!
          (list 'quote (car (cdr form)))
          (car (cdr (cdr form))))))
(def-toplevel-macro!
  'define-macro
  (lambda (form)
    (list 'def-toplevel-macro!
          (list 'quote (cadr form))
          (caddr form))))

(define apply (lambda (fn args) (letcc k ((apply-ktx k fn args)))))


(define mapn
  (lambda (fn lsts)
    (if (null? (car lsts)) ;; assume all same length
        '()
        (cons (apply fn (map1-prim car lsts))
              (mapn fn (map1-prim cdr lsts))))))
(define map
  (lambda (fn . lsts)
    (mapn fn lsts)))


(define list* (lambda (a . rest)
                (if (null? rest) a
                    (cons a (apply list* rest)))))
(define-macro let
  (lambda (form)
    ((lambda (names values body)
       (list* (list* 'lambda names body) values))
     (map1 car (cadr form))
     (map1 cadr (cadr form))
     (cddr form))))

(define-macro letrec
  (lambda (form)
    (let ((bindings (cadr form)))
      (let ((names (map1 car bindings))
            (values (map1 cadr bindings)))
        (list* 'let (map1 (lambda (n) (list n #f)) names)
               (cons 'begin (map (lambda (n v) (list 'set! n v)) names values))
               (cddr form))))))

;; Overwrite define + define-macro with more complete implementations.
(define make-definer
  (lambda (def!)
    (lambda (form)
      (let ((name (cadr form))
            (value (caddr form)))
        (if (pair? name)
            (let ((_name (car name))
                  (_formals (cdr name)))
              (set! name _name)
              (set! value (list* 'lambda _formals (cddr form)))))
        (list def! (list 'quote name) value)))))

(define-macro define (make-definer 'def-toplevel!))
(define-macro define-macro (make-definer 'def-toplevel-macro!))

(define (procedures) (map1 car (toplevel)))
(define (macros) (map1 car (toplevel-macro)))

(define (with-letform-transpose bindings_body fn)
  (fn (map1 car (car bindings_body))
      (map1 cadr (car bindings_body))
      (cdr bindings_body)))

;; Redefine let with named let.
(define-macro (let form)
  (if (symbol? (cadr form))
      ;; named let
      (with-letform-transpose
       (cddr form)
       (lambda (names values body)
         (let ((name (cadr form)))
           (list 'letrec
                 (list (list name (list* 'lambda names body)))
                 (cons name values)))))
      ;; normal let
      (with-letform-transpose
       (cdr form)
       (lambda (names values body)
         (list* (list* 'lambda names body) values)))))

;; (or a b)  -> (if a a b)   
;; (and a b) -> (if a b a)

(define-macro (or form)
  (let clause ((args (cdr form)))
    (if (null? (cdr args)) (car args)
        (list 'if (car args)
              (car args)
              (clause (cdr args))))))
  
(define-macro (and form)
  (let clause ((args (cdr form)))
    (if (null? (cdr args)) (car args)
        (list 'if (car args) 
              (clause (cdr args))
              (car args)))))



(define (dbg x) (post x) x)
(define-macro (cond form)
  (let next ((f (cdr form)))
    (if (null? f) '(void)
        (let ((c  (car f)))
          (if (eq? 'else (car c))
              (cadr c)
              (list 'if (car c) (cadr c)
                    (next (cdr f))))))))
(define-macro (*** form)
  (list 'begin
        (list 'post (list 'quote (cdr form)))
        (list 'write ''=>)
        (list 'dbg (cdr form))))

(define (void . _) (make-void))

(define-macro (when form)
  (list 'if (cadr form)
        (cons 'begin (cddr form))
        '(void)))
(define-macro (unless form)
  (list 'if (cadr form) '(void)
        (cons 'begin (cddr form))))

(define (open-output-file filename) (open-mode-file filename "w"))
(define (open-input-file filename) (open-mode-file filename "r"))

;; misc r4rs
(define (append2 a b)
  (if (null? a) b (cons (car a) (append2 (cdr a) b))))
(define (append-lists a)
  (if (null? a) '()
  (if (null? (cdr a)) (car a)
  (if (null? (cddr a)) (append2 (car a) (cadr a))
      (append2 (car a) (append-lists (cdr a)))))))
;; (define append append2)
(define (append . lsts) (append-lists lsts))

(define-macro (quasi-quote x)
  (let qq ((expr (cadr x)))
    (cond
     ((list? expr)
      (cond
       ((eq? 'unquote (car expr)) (cadr expr))
       ((eq? 'unquote-spicing (car expr)) (syntax-error x))
       (else
        (cons 'append
          (map (lambda (el)
                (or
                 (and (pair? el)
                 (cond
                  ((eq? 'unquote (car el)) (list 'list (cadr el)))
                  ((eq? 'unquote-splicing (car el)) (cadr el))
                  (else #f)))
               (list 'list (list 'quote el))))
            expr)))))
     ((pair? expr)
      (list 'cons (qq (car expr)) (qq (cdr expr))))
     (else
      (list 'quote expr)))))

(define (list-tail lst k) (if (zero? k) lst (list-tail (cdr lst) (sub1 k))))
(define (list-ref lst k)  (if (zero? k) (car lst) (list-ref (cdr lst) (sub1 k))))

(define (make-mem eq?)
  (lambda (obj lst)
    (let next ((lst lst))
      (if (null? lst) #f
          (if (eq? (car lst) obj) lst
              (next (cdr lst)))))))
(define member (make-mem equal?))
(define memq (make-mem eq?))

(define (make-assoc eq?)
  (lambda (obj lst)
    (let next ((lst lst))
      (if (null? lst) #f
          (if (eq? (caar lst) obj) (car lst)
              (next (cdr lst)))))))
(define assoc (make-assoc equal?))
; (define assq (make-assoc eq?))
(define assv assq) ;; FIXME

(define (make-accu op init)
  (lambda lst
    (let next ((acc init)
               (lst lst))
      (if (null? lst) acc
          (next (op acc (car lst)) (cdr lst))))))

(define + (make-accu add 0))
(define * (make-accu mul 1))
;; (define - (make-accu sub 0))
(define - sub)

(define = eq)
(define > gt)
(define < lt)
(define (>= a b) (not (< a b)))
(define (<= a b) (not (> a b)))

(define (make-min-max rel)
  (lambda lst
    (let next ((lst (cdr lst))
               (el (car lst)))
      (if (null? lst) el
          (next (cdr lst)
                (if (rel el (car lst)) el (car lst)))))))

(define max (make-min-max >))
(define min (make-min-max <))

(define (procedure? fn) (or (prim? fn) (lambda? fn)))



;; for Shivers SRFI-1
(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))



;(define-macro (quasiquote x)
;  (let rec ((x x))
;    (if (pair? x)
;        (if (eq? 'unquote (car x)) 



;; R4RS
(define char? integer?)
(define number? integer?)
(define string? bytes?)
(define (for-each . args) (void (apply map args)))

(define (make-string n . fill)
  (let ((b (make-bytes n)))
    (bytes-init b (if (null? fill) 0 (car fill))) b))

(define (make-writer write-port)
  (lambda (it . port)
    (if (null? port)
        (write-port it (current-output-port))
        (write-port it (car port)))))

(define write (make-writer write-port))
(define display (make-writer write-bytes))

(define (newline . port)
  (let ((cr "\n"))
    (apply display (cons cr port))))



;; parser might trigger GC, so we call it manually before reading.
;; FIXME: at least make this detectable!!
(define (read-gc port) (gc) (read-no-gc port))
(define (read . port)
  (gc)
  (if (null? port)
      (read-gc (current-input-port))
      (read-gc (car port))))

(define (load filename)
  (let ((port (open-input-file filename)))
    (let next ((last (void)))
      (let ((expr (read port)))
        (if (eof-object? expr)
            last
            (next (eval expr)))))))

;; Run repl until end-of-input.  On error the abort continuation is
;; invoked (see 'abort-k!').
(define (repl-no-guard display-prompt exit-repl)
  (let loop ()
    (display-prompt)
    (let ((expr (read (current-input-port))))
      (if (eof-object? expr)
          (exit-repl)
          (begin
            (let ((val (eval expr)))
              (unless (void? val)
                (write val) (newline)
                (flush-output-port (current-output-port))))
            (loop))))))

;; Run repl, abort on error or EOF.  Perform collection before halt.
;; This is used in conjuction with _sc_repl_cstring().
(define (repl-oneshot)
  (letcc k (begin
             (abort-k! k)
             (repl-no-guard void void)))
  (gc))

(define (repl)
  ;; (display "libprim/SC") (newline)
  (let loop ()
    (print-error
     (letcc k (begin
                (abort-k! k)
                (repl-no-guard
                 (lambda ()
                   (display "> " (current-error-port))
                   (flush-output-port (current-error-port)))
                 exit))))
    (flush-output-port (current-error-port))
    (loop)))


(define (read-string str) (read (open-input-string str)))
(define (eval-string str) (eval (read-string str)))

(define (make-global-access n)
  (lambda p (if (null? p) (global n) (set-global! n (car p)))))

;; See scheme.h -> sc_slot_*
(define current-input-port  (make-global-access 4))
(define current-output-port (make-global-access 5))
(define current-error-port  (make-global-access 6))


(define (repl-on-ports in out err)
  (current-input-port  in)
  (current-output-port out)
  (current-error-port err)
  (repl))

;; Dial into a remote console server (i.e. "netcat -l -p 12345")
(define (repl-connect host port)
  (let ((ports (tcp-connect host port)))
    (repl-on-ports (car ports) (cdr ports) (cdr ports))))

;; Start a (one-shot) console server.
(define (repl-serve port)
  (let ((fd (tcp-bind "0.0.0.0" port)))
    (let ((ports (tcp-accept fd)))
      (repl-on-ports (car ports) (cdr ports) (cdr ports)))))

;(let ((x (read (open-input-file "boot.scm"))))
;  (let loop ((n 40))
;    (unless (zero? n)
;      (expand x) (loop (sub1 n)))))


;; (display "libprim/SC\n")
;; (repl)

;; Collect before re-entering C.
(gc))))

