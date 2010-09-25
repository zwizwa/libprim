
;;; Bootstrap

;;;

;; The top level boot expression is of the form reflecting 2 phases:
;;     `(begin
;;        (begin . ,<boot-1-exprs>)
;;        (eval-list (quote ,<boot-2-exprs>)))
;;
;; The <boot-1-exprs> bootstraps the macro expander `eval' in terms of
;; the macro-less `eval-core' while <boot-2-exprs> is defined in terms
;; of the macro-extended language.

;; Code that processes this file other than the macro-less interpreter
;; relies on this structure, so don't change it!

;; The main reason for this 2-phase approach is to keep the macro
;; expander out of the core VM avoiding the need for recursion and
;; garbage collection in C.

(begin

;;; Macro expander bootstrap.
(begin

(def-toplevel! 'assq
  (%lambda (obj lst)
    (if (null? lst) #f
        (if (eq? (caar lst) obj) (car lst)
            (assq obj (cdr lst))))))
(def-toplevel! 'map1
  (%lambda (fn lst)
    (if (null? lst) lst
        (cons (fn (car lst))
              (map1 fn (cdr lst))))))
(def-toplevel! 'expand-lambda
  (%lambda (expr expand)
    (cons '%lambda
    (cons (cadr expr)
    (map1 expand (cddr expr))))))
(def-toplevel! 'expand
  (%lambda (expr)
    (if (pair? expr)
        ((%lambda (tag)
           (if (eq? tag 'quote) expr
           (if (eq? tag 'lambda) (expand-lambda expr expand)
               ((%lambda (rec)
                  (if rec
                      (expand ((cdr rec) expr))
                      (map1 expand expr)))
                (assq (car expr) (toplevel-macro))))))
         (car expr))
        expr)))
(def-toplevel! 'eval
  (%lambda (expr) (eval-core (expand expr))))

;; Evaluate expressions in sequence.  This makes sure macros take
;; effect immediately after definition so they can be used in the next
;; expression.
(def-toplevel! 'eval-list
  (%lambda (expr)
    (if (null? expr) (void)
        (begin (eval (car expr)) (eval-list (cdr expr))))))

)
;;; Library with macro expansion.

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

(define apply apply1)


(define mapn
  (lambda (fn lsts)
    (if (null? (car lsts)) ;; assume all same length
        '()
        ((lambda (head) ;; (*)
           (cons head (mapn fn (map1-prim cdr lsts))))
         (apply1 fn (map1-prim car lsts))))))

;; (*) to implement `for-each' in terms of `map', it's simplest to
;; make sure that `fn' in `mapn' is applied from left to right.  Note
;; that all application forms (like `cons' above) evaluate from right
;; to left.

(define map
  (lambda (fn . lsts)
    (mapn fn lsts)))

(define list* (lambda (a . rest)
                (if (null? rest) a
                    (cons a (apply list* rest)))))
;; This let is in terms of lambda.
(define expand-%let
  (lambda (form)
    ((lambda (names values body)
       (list* (list* 'lambda names body) values))
     (map1 car (cadr form))
     (map1 cadr (cadr form))
     (cddr form))))

(define-macro let expand-%let)
(define-macro %let expand-%let)

;; (%load "boot1-expand-letrec.scm")
(define expand-letrec
  (lambda (form)
    (let ((bindings (cadr form)))
      (let ((names (map1 car bindings))
            (values (map1 cadr bindings)))
        (list* 'let (map1 (lambda (n) (list n #f)) names)
               (cons 'begin (map (lambda (n v) (list 'set! n v)) names values))
               (cddr form))))))


(define-macro letrec expand-letrec)

;; Overwrite define + define-macro with more complete implementations.
;; (%load "boot1-expand-define.scm")
;; Convert define syntax into (list symbol form)
(define expand-define
  (lambda (form)
    (let ((name (cadr form))
          (value (caddr form)))
      (if (pair? name)
          (let ((_name (car name))
                (_formals (cdr name)))
            (set! name _name)
            (set! value (list* 'lambda _formals (cddr form))))
          #f)
      (list name value))))
(define make-definer
  (lambda (def!)
    (lambda (form)
      (let ((n+v (expand-define form)))
        (list def! (list 'quote (car n+v)) (cadr n+v))))))

(define-macro define (make-definer 'def-toplevel!))
(define-macro define-macro (make-definer 'def-toplevel-macro!))

;; Redefine let with named let.
;; (%load "boot1-expand-let.scm")
(define (with-letform-transpose bindings_body fn)
  (fn (map1 car (car bindings_body))
      (map1 cadr (car bindings_body))
      (cdr bindings_body)))

(define (expand-let form)
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
      (cons '%let (cdr form))))

(define-macro let expand-let)

;; Support internal definitions
;; (%load "boot1-expand-lambda.scm")
;; Support internal definitions
(define (expand-lambda lambda-form expand)
  (let ((define? (lambda (x)
                   (if (pair? x)
                   (if (pair? (car x))
                   (eq? 'define (caar x))
                   #f) #f))))
    (let collect ((body (cddr lambda-form))
                  (defs '()))
      (if (define? body)
          (collect (cdr body)
                   (cons (expand-define (car body)) defs))
          (let ((defs (reverse defs)))
            (list* '%lambda
                   (cadr lambda-form)
                   (if (null? defs)
                       (map expand body)
                       (list (expand (list* 'letrec (reverse defs) body))))))))))

;; (%load "boot2.scm")

;; Second stage bootstrap.

(define (vars) (map1 car (toplevel)))
(define (macros) (map1 car (toplevel-macro)))

;; (or a b)  -> (if a a b)   
;; (and a b) -> (if a b a)

(define-macro (or form)
  (let clause ((args (cdr form)))
    (if (null? args) #t
        (if (null? (cdr args)) (car args)
            (list 'if (car args)
                  (car args)
                  (clause (cdr args)))))))
  
(define-macro (and form)
  (let clause ((args (cdr form)))
    (if (null? args) '#f
        (if (null? (cdr args)) (car args)
            (list 'if (car args) 
                  (clause (cdr args))
                  (car args))))))

(define (dbg x) (post x) x)
(define-macro (cond form)
  (let next ((f (cdr form)))
    (if (null? f) '(void)
        (let ((c  (car f)))
          (if (eq? 'else (car c))
              (cons 'begin (cdr c))
              (list 'if (car c)
                    (cons 'begin (cdr c))
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
(define (open-output-tempfile) (open-mode-tempfile (bytes-copy "tmp-XXXXXX") "w"))

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

; (define (complex? x) #f)
; (define real? inexact?)

(define (error tag ob)
  (raise-error tag ob))

;; taken from TinyScheme's init.scm:
;;
;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;;
;; Subsequently modified to handle vectors: D. Souflis

(define (expand-quasiquote l)
  (letrec
      ((mcons
        (lambda (f l r)
          (if (and (pair? r)
                   (eq? (car r) 'quote)
                   (eq? (car (cdr r)) (cdr f))
                   (pair? l)
                   (eq? (car l) 'quote)
                   (eq? (car (cdr l)) (car f)))
              (if (or (procedure? f) (number? f) (string? f))
                  f (list 'quote f))
              ;(if (eqv? l vector)
              ;    (apply l (eval r))
                  (list 'cons l r)
                  )))
       (mappend
        (lambda (f l r)
          (if (or (null? (cdr f))
                  (and (pair? r)
                       (eq? (car r) 'quote)
                       (eq? (car (cdr r)) '())))
              l (list 'append l r))))
       (tx
        (lambda (level form)
          (cond ((not (pair? form))
                 (if (or (procedure? form) (number? form) (string? form))
                     form (list 'quote form)))
                ((eq? 'quasiquote (car form))
                 (mcons form ''quasiquote (tx (+ level 1) (cdr form))))
                (#t (if (zero? level)
                        (cond ((eq? (car form) 'unquote) (car (cdr form)))
                              ((eq? (car form) 'unquote-splicing)
                               (error "Unquote-splicing wasn't in a list" form))
                              ((and (pair? (car form))
                                    (eq? (car (car form)) 'unquote-splicing))
                               (mappend form (car (cdr (car form)))
                                        (tx level (cdr form))))
                              (#t (mcons form (tx level (car form))
                                         (tx level (cdr form)))))
                        (cond ((eq? (car form) 'unquote)
                               (mcons form ''unquote (tx (- level 1) (cdr form))))
                              ((eq? (car form) 'unquote-splicing)
                               (mcons form ''unquote-splicing
                                      (tx (- level 1) (cdr form))))
                              (#t (mcons form (tx level (car form))
                                         (tx level (cdr form)))))))))))
    (tx 0 (car (cdr l)))))


(define-macro quasiquote expand-quasiquote)

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
(define memv (make-mem eqv?))

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

;; Like left fold, but without NIL
(define (bin-reduce binop unit lst)
  (cond
   ((null? lst) unit)
   ((null? (cdr lst)) (car lst))
   (else
    (let rec ((lst (cdr lst))
              (acc (car lst)))
      (if (null? lst) acc
          (rec (cdr lst) (binop acc (car lst))))))))

(define (+ . a) (bin-reduce add 0 a))
(define (* . a) (bin-reduce mul 1 a))
;; (define - (make-accu sub 0))
(define - sub)
(define / div)

(define (= . args)
  (let rec ((args args))
    (let ((x1 (car args))
          (x2 (cadr args))
          (rest (cddr args)))
      (if (null? rest)
          (eq x1 x2)
          (and (eq x1 x2) (rec (cdr args)))))))

  
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
(define (negative? x) (< x 0))
(define (abs x) (if (negative? x) (* -1 x) x))

(define (procedure? fn) (or (prim? fn) (lambda? fn)))

(define (make-vector n . args)
  (make-vector/init n (if (null? args) 0 (car args))))

(define (vector . args) (list->vector args))

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

;; This relies on the order of map.
(define (for-each . args) (void (apply map args)))


(define string=? string-equal?)
(define string-length bytes-length)
(define string-set! bytes-set!)
(define string-ref bytes-ref)
(define (string . args) (vector->bytes (list->vector args)))

(define (make-string n . fill)
  (let ((b (make-bytes n)))
    (bytes-init b (if (null? fill) 0 (car fill))) b))

(define (make-writer write-port)
  (lambda (it . port)
    (if (null? port)
        (write-port it (current-output-port))
        (write-port it (car port)))))

(define write (make-writer write-port))
(define display (make-writer
                 (lambda (it port)
                   ((if (bytes? it) write-bytes write) it port))))

(define (newline . port)
  (let ((cr "\n"))
    (apply display (cons cr port))))

(define (make-global-access n)
  (lambda p (if (null? p) (global n) (set-global! n (car p)))))

;; See scheme.h -> sc_slot_*
(define current-input-port  (make-global-access 3))
(define current-output-port (make-global-access 4))
(define current-error-port  (make-global-access 5))


(define close-output-port close-port)
(define close-input-port close-port)

(define (foldr1 fn init lst)
  (let rec ((lst lst))
    (if (null? lst) init
        (fn (car lst) (rec (cdr lst))))))

(define-macro (let* form)
  (let ((bindings (cadr form))
        (body (cons 'begin (cddr form))))
    (foldr1 (lambda (clause body)
              `(let (,clause) ,body))
            body
            bindings)))

(define (cadar x) (car (cdar x)))

(define-macro (record-case form)
  (let ((bind-args
         (lambda (vars rest-name form)
           (foldr1 (lambda (var body)
                     `(let ((,var (car ,rest-name))
                            (,rest-name (cdr ,rest-name)))
                        ,body))
                   form vars)))
        (expr (cadr form))
        (e-val    '_e-val)      ;; FIXME: use gensym
        (rec-tag  '_rec-tag)
        (rec-args '_rec-args))
    `(let* ((,e-val ,expr)
            (,rec-tag (car ,e-val))
            (,rec-args (cdr ,e-val)))
       ,(let rec ((clauses (cddr form)))
          (cond
           ((null? clauses) '(void))
           ((eq? 'else (caar clauses)) (cadar clauses))
           (else
            (let* ((clause (car clauses))
                   (tag  (car clause))
                   (args (cadr clause))
                   (body (caddr clause)))
              `(if (eqv? ',tag ,rec-tag)  ;; NOTE: doesn't use eq?
                   ,(bind-args args rec-args body)
                   ,(rec (cdr clauses))))))))))

;; Re-implement some macros to be more generic using record-case and
;; quasiquote.

(define (expand-cond form)
  (let next ((clauses (cdr form)))
    (if (null? clauses) '(void)
        (let* ((clause (car clauses))
               (rest (lambda () (next (cdr clauses))))
               (guard (car clause))
               (body (cons 'begin (cdr clause))))
          (cond
           ((eq? 'else guard) body)
           ((null? (cdr clause))
            `(if ,guard ,(void) ,(rest)))
           ((eq? '=> (cadr clause))
            (let ((body (caddr clause)))
              `(let ((bv ,guard))
                 (if bv (,body bv) ,(rest)))))
           (else
            `(if ,guard ,body ,(rest))))))))

(define-macro cond expand-cond)


(define (improper lst)
  (if (null? lst) '()
      (let rec ((lst lst))
        (if (null? (cdr lst))
            (car lst)
            (cons (car lst) (rec (cdr lst)))))))
                               
(define (apply fn . args)
  (apply1 fn (improper args)))
        
(define (string-append . args)
  (bytes-vector-append (list->vector args)))

(define-macro (case form)
  (let ((expr (cadr form))
        (ev '_ev)) ;; FIXME: use gensym
    `(let ((,ev ,expr))
       ,(let rec ((clauses (cddr form)))
          (if (null? clauses) '(void)
              (let* ((clause (car clauses))
                     (set (car clause))
                     (action (cadr clause)))
                (if (eq? 'else set)
                    action
                    `(if (memq ,ev ',set) ,action
                         ,(rec (cdr clauses))))))))))

;; From TinyScheme init.scm    
(define-macro (do form)
  (apply
   (lambda (do vars endtest . body)
     (let ((do-loop '_do-loop)) ;; FIXME: use gensym
       `(letrec ((,do-loop
                  (lambda ,(map (lambda (x)
                                  (if (pair? x) (car x) x))
                                `,vars)
                    (if ,(car endtest)
                        (begin ,@(cdr endtest))
                        (begin
                          ,@body
                          (,do-loop
                           ,@(map (lambda (x)
                                    (cond
                                     ((not (pair? x)) x)
                                     ((< (length x) 3) (car x))
                                     (else (car (cdr (cdr x))))))
                                  `,vars)))))))
          (,do-loop
           ,@(map (lambda (x)
                    (if (and (pair? x) (cdr x))
                        (car (cdr x))
                        '()))
                  `,vars)))))
   form))

(define (load-lib filename)
  (load (string-append (script-dir) filename)))



;; R4RS test hacks
(define (complex? x) #f)
(define (rational? x) #f)
(define real? number?)
(define exact? integer?)


(define (expt a b) (exp (* (log a) b)))


;;; Console & IO

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

         
           
(define (script)
  (let ((file (car args)))
    (set! args (cdr args)) ;; necessary?  maybe leave script name?
    (load file)))

(define repl-prompt
  ;; "> "
  "OK\n"
  )
(define (repl)
  ;; (display "libprim/SC") (newline)
  (let loop ()
    (print-error
     (letcc k (begin
                (abort-k! k)
                (repl-no-guard
                 (lambda ()
                   (display repl-prompt (current-error-port))
                   (flush-output-port (current-error-port)))
                 exit))))
    (flush-output-port (current-error-port))
    (loop)))

(define (repl-on-ports in out err)
  (current-input-port  in)
  (current-output-port out)
  (current-error-port err)
  (repl))


(define (read-string str) (read (open-input-string str)))
(define (eval-string str) (eval (read-string str)))


;; Multihead


;; Dial into a remote console server (i.e. "netcat -l -p 12345")
(define (repl-connect host port)
  (let ((ports (tcp-connect host port)))
    (repl-on-ports (car ports) (cdr ports) (cdr ports))))

;; Start a (one-shot) console server.
(define (repl-serve-accept fd)
  (let ((ports (socket-accept fd)))
    (repl-on-ports (car ports) (cdr ports) (cdr ports))))
(define (repl-serve-tcp tcp-port)
  (repl-serve-accept (tcp-bind "0.0.0.0" tcp-port)))
(define (repl-serve-unix node)
  (repl-serve-accept (unix-bind node #t)))

;
(define (action-ready a) (vector-ref a 2))
(define (action-thunk a) (vector-ref a 3))

(define-macro (push! form)
  (let ((var (cadr form))
        (val (caddr form)))
    `(set! ,var (cons ,val ,var))))

  
;; Eval / error.
;; FIXME: this uses the global abort handler.  Use params!
(define (rpc-eval expr)
  (let* ((result #f)
         (err (letcc k
                     (begin
                       (abort-k! k)
                       (set! result `(ok ,(eval expr)))
                       #f))))
    (or result `(error ,(cddr (vector->list (struct->vector err)))))))


(define (filter fn lst)
  (let rec ((lst lst))
    (cond ((null? lst) '())
          ((fn (car lst)) (cons (car lst) (rec (cdr lst))))
          (else (rec (cdr lst))))))

(define (console-dispatch fd io-list poll)
  (let* ((actions '())
         (remove-io-action!
          (lambda (io)
            ;; (display "close conn: ") (write io) (newline)
            (close-output-port (cdr io))
            (close-input-port (car io))
            (set! actions (filter
                           (lambda (a) (not (equal? (vector-ref a 0) (car io))))
                           actions))))
         (add-io-action!
          (lambda (io)
            ;; (display "conn: ") (write io) (newline)
            (push! actions
              (vector (car io) 0 ;; sync on input invents
                      #f         ;; initial condition is false
                      (lambda ()
                        (let ((expr (read (car io))))
                          (if (eof-object? expr)
                              (remove-io-action! io)
                              (let ((val (rpc-eval expr)))
                                (write val (cdr io))
                                (newline (cdr io))
                                (flush-output-port (cdr io))))))))))
         (accept-thunk
          (lambda () (add-io-action! (socket-accept fd)))))
      ;; Add listener & io ports
      (push! actions (vector fd 0 #f accept-thunk))
      (for-each (lambda (io) (add-io-action! io)) io-list)
      
      ;; Dispatch loop
      (let loop ()
        (select! actions 0.100)
        (for-each (lambda (a)
                    (when (action-ready a) ((action-thunk a))))
                  actions)
        (poll)
        (loop))))


;; Start a unix socket server + stdio dispatcher.
(define (console-socket fd)
  (console-dispatch fd
                    (list (cons (current-input-port)
                                (current-output-port)))
                    void))

(define (unix-console-server node) (console-socket (unix-bind node #t)))
(define (tcp-console-server prt)   (console-socket (tcp-bind "0.0.0.0" prt)))


(define (init-console io node)
  (write node) (newline)
  (console-dispatch
   (if (list? node)
       (apply tcp-bind node)
       (unix-bind (or node "/tmp/sc") #t))
   (list io)
   void))

(define (undefined x) (error 'undefined x))


;; FIXME: move this to a separate file.
;;; JAVA 
;(define-macro (j form)
;  `(java-call
;    (vector ,(symbol->string (cadr form))
;            (vector ,@(cddr form)))))


(gc)

)))

