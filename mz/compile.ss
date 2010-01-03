#lang scheme/base
(require scheme/include
         scheme/pretty
         (planet dvanhorn/record-case:1:1/record-case))

(define (op-if . a)     (cons "if" a))
(define (op-ref . a)    (cons "ref" a))
(define (op-lit . a)    (cons "lit" a))
(define (op-let . a)    (cons "let" a))
(define (op-app . a)    (cons "app" a))
(define (op-seq . a)    (cons "seq" a))
(define (op-lambda . a) (cons "lambda" a))
(define (op-assign . a) (cons "assign" a))

(define (vm-init expr)
  (pretty-print expr))
(define (vm-continue) (void))

(define (undefined x) (error 'undefined (symbol->string x)))

(include "compile.scm")
