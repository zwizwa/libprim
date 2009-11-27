;; select() based server for multi-headed console.


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
(define (repl-serve-accept fd)
  (let ((ports (socket-accept fd)))
    (repl-on-ports (car ports) (cdr ports) (cdr ports))))
(define (repl-serve-tcp tcp-port) (repl-serve-accept (tcp-bind "0.0.0.0" tcp-port)))
(define (repl-serve-unix node) (repl-serve-accept (unix-bind node #t)))

;
(define (action-ready a) (vector-ref a 2))
(define (action-thunk a) (vector-ref a 3))

(define-macro (push! form)
  (let ((var (cadr form))
        (val (caddr form)))
    `(set! ,var (cons ,val ,var))))
               

(define (console-dispatch fd)
  (let ((actions '())
        (make-io-action
         (lambda (io)
           (vector (car io) 0 ;; sync on input invents
                   #f         ;; initial condition is false
                   (lambda ()
                     (let* ((expr (read (car io)))
                            (val (eval expr)))
                       (write val (cdr io))
                       (newline (cdr io))
                       (flush-output-port (cdr io))))))))
    (let ((accept-thunk
           (lambda ()
             (let ((io (socket-accept fd)))
               (push! actions (make-io-action io))))))
      (set! actions (list (vector fd 0 #f accept-thunk)))
      (let loop ()
        (select! actions #f)
        (for-each (lambda (a)
                    (when (action-ready a) ((action-thunk a))))
                  actions)
        (loop)))))
      
