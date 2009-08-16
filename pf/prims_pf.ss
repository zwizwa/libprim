#lang scheme/base

(require "../prim-tools.ss")

;; Bootstrap primitive init from C file.
(parameterize
    ((re-def  (pregexp "void\\s+?pf_\\S*?\\(pf\\s*?\\*.*?\\)"))
     (re-name (pregexp "pf_\\S*?(?=\\()"))
     (ctx "pf")
     (macro-prefix "PF_") ;; stack semantics (no prefix = EX semantics)
     )
  (gen))


