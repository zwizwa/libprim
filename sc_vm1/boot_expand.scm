;; Expansion of the boot.scm file for inclusion in Flash based
;; microcontrollers.

;; This file is based on the structure of the boot file, which
;; consists of two phases:
;;
;;  * boot-1: Bootstrap of the macro expander `eval' in terms of `eval-core'.
;;  * boot-2: Evaluation of the remainder of the code in terms of `eval'.

;; The top level boot expression is of the form:
;;    (begin
;;        (begin . <boot-1-exprs>)
;;        (eval-list (quote <boot-2-exprs>)))
;;

(define (boot-expand boot)
  (let*
      ;; Decompose expresssion in 2 components.
      ((boot-1 (cdr  (cadr boot)))
       (boot-2 (cadr (cadr (caddr boot)))))
    ;; Generate an expression with the same structure to make
    ;; boot-expand idempotent.
    `(begin
       (begin
         ,@(append boot-1 (map expand boot-2)))
       (eval-list '()))))

(define (boot-translate infile outfile)
  (let ((outport (open-output-file outfile))
        (inport  (open-input-file  infile)))
    (write (boot-expand (read inport)) outport)
    (newline outport)
    (close-port outport)
    (close-port inport)))


;; For script:
(define main boot-translate)

