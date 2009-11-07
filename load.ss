#lang scheme/base


(require "unparse.ss")
(current-input-port
 (open-input-file
  (vector-ref
   (current-command-line-arguments) 0)))
(convert)


