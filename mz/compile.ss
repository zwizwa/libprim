#lang scheme/base
(require scheme/include
         scheme/pretty)
;;         (planet dvanhorn/record-case:1:1/record-case))

(define-syntax-rule (define-ops op ...)
  (begin (define (op . a) (list->vector (cons 'op a))) ...))
(define-ops
  op-if
  op-ref
  op-lit
  op-app
  op-seq
  op-let
  op-lambda
  op-assign)

(define (vm-init expr)
  (pretty-print expr))
(define (vm-continue) (void))

(define (undefined x) (error 'undefined (symbol->string x)))

(include "compile.scm")
