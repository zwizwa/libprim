;;; FFMPEG CONSOLE APP


(define (any->string it)
  (cond
   ((symbol? it) (symbol->string it))
   ((number? it) (number->string it))
   ((string? it) it)
   (else (error "Can't convert to string" it))))

(define (build-args app options args)
   (list->vector
    (append
     (list (any->string app))
     (apply append
            (map (lambda (rec)
                   (let ((opt (string-append "-" (any->string (car rec)))))
                     (if (cdr rec)
                         (list opt (any->string (cdr rec)))
                         (list opt))))
                 options))
     (map any->string args))))


(define (ffmpeg options . args)
  (let ((args (build-args 'ffmpeg options args)))
    (begin (write args) (newline))
    (open-output-process args)))


(define (times n thunk)
  (let rec ((n n))
    (unless (zero? n) (thunk) (rec (sub1 n)))))


;; Write out an mpeg4 encoded avi file with garbage from memory.
(define (test-mpeg-avi filename)
  (define (testframe w h)
    (let* ((plane (* w h))
           (size (+ plane (/ plane 2))))
      (make-bytes size)))
  (define (open-output-mpeg4 filename)
    (ffmpeg
     `(;; input options
       (r . 25)
       (s . "320x240")
       (f . rawvideo)
       (i . pipe:.yuv)
       ;; output options
       (y . #f)           ;; force overwrite output
       (f . avi)
       (vcodec . mpeg4)
       ) filename))
  (let ((port (open-output-mpeg4 filename))
        (frame (testframe  320 240)))
    (times 100 (lambda () (write-bytes frame port)))
    (close-port port)))

