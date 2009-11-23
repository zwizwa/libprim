;;; FFMPEG CONSOLE APP

(load "media.scm")

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

(define (res w h)
  (string-append (number->string w) "x" (number->string h)))


(define (open-output-mpeg4 filename)
  (ffmpeg
   `(;; input options
     (r . 25)
     (s . ,(res 320 240))
     (f . rawvideo)
     (i . pipe:.yuv)
     ;; output options
     (y . #f)           ;; force overwrite output
     (f . avi)
     (vcodec . mpeg4)
     ) filename))

(define (open-output-wav filename)
  (ffmpeg
   `((ac . 2)     ;; channels
     (ar . 44100) ;; rate
     (f . s16le)
     (i . pipe:.pcm)
     (y . #f))
   filename))

(define (open-output-mp3 filename)
  (open-output-process
   (build-args 'lame '((s . 44100)
                       (r . #f))      ;; raw
                       
               (list "-" filename))))

(define (open-input-audio filename)
  (let ((args (build-args 'ffmpeg
                          `((i . ,filename)
                            (f . s16le))
                          '("-"))))
    (open-input-process args)))


;; Write out an mpeg4 encoded avi file with garbage from memory.
(define (test-mpeg-avi filename)
  (let ((port (open-output-mpeg4 filename))
        (frame (make-yuv 320 240 "I420")))
    (times 100 (lambda () (write-bytes frame port)))
    (close-port port)))


;; AUDIO MODULATION / DEMODULATION proof of concept.
;; FIXME: make interface a bit easier to use

;; Write out a signal sweep as .mp3

(define (write-test-sweep-port port)
  (let ((v (hankel (signal-sweep 4000. .0
                                 .01 .11 441000) 2)))
    (grid-write-short v port)
    (close-port p)))
(define (write-test-sweep-mp3 filename)
  (write-test-sweep-port (open-output-mp3 filename)))

;; Read from mp3 and print out the detected frequency.
(define (open-test-sweep-mp3 filename)
  (let ((p (open-input-audio filename))
        (g (make-grid-2 2 1024 .0)))
    (let rec ()
      (grid-read-short g p)
      (write (signal-angle (grid-column g 0))) (newline)
      (rec))))
            
    
        


