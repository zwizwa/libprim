#lang scheme/base
(require scheme/include)

(define vm-init void)
(define vm-continue void)
(define (vm-compile-anf x) x)

(define (undefined x) (error 'undefined (symbol->string x)))
(define map1 map)
  
(include "../sc/boot1-expand-let.scm")
(include "../sc/boot1-expand-letrec.scm")
(include "../sc/boot1-expand-lambda.scm")
(include "../sc/boot1-expand-define.scm")
(include "compile.scm")
