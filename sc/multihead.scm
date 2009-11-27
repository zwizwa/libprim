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
               

(define (console-dispatch fd io-list poll)
  (let* ((actions '())
         (add-io-action!
          (lambda (io)
            (push! actions
              (vector (car io) 0 ;; sync on input invents
                      #f         ;; initial condition is false
                      (lambda ()
                        (let* ((expr (read (car io)))
                               (val (eval expr)))
                          (write val (cdr io))
                          (newline (cdr io))
                          (flush-output-port (cdr io))))))))
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

(define (unix-server node poll)
  (console-dispatch
   (unix-bind node #t)
   (list (cons (current-input-port)
               (current-output-port)))
   poll))