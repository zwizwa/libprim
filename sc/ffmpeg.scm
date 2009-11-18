;;; FFMPEG CONSOLE APP


(define (any->string it)
  (cond
   ((symbol? it) (symbol->string it))
   ((number? it) (number->string it))
   ((string? it) it)
   (else (error "Can't convert to string" it))))

(define (dict->args app options . args)
   (list->vector
    (append
     (list (any->string app))
     (apply append
            (map (lambda (rec)
                   (list (string-append "-" (any->string (car rec)))
                         (any->string (cdr rec))))
                 options))
     args)))

