#lang scheme/base

(require "../prim-tools.ss")

;; Bootstrap primitive init from C file.
(parameterize
    ((re-def  (pregexp "_\\s+?ex_\\S*?\\(ex\\s*?\\*.*?\\)"))
     (re-name (pregexp "ex_\\S*?(?=\\()"))
     (ctx "EX"))
  (gen))


