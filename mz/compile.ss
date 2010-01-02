#lang scheme/base
(require scheme/include
         scheme/pretty
         (planet dvanhorn/record-case:1:1/record-case))


(define (op-ref)  "ref")
(define (op-lit)  "lit")
(define (op-let1) "let1")
(define (op-app)  "app")

(define (vm-init expr)
  (pretty-print expr))
(define (vm-continue) (void))

(define (undefined x) (error 'undefined (symbol->string x)))

(include "compile.scm")
