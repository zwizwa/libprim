#lang scheme/base

;; Bootstrap primitive init from C file.
(require "../prim-tools.ss")


(ex-gen "sc")

;; (parameterize
;;     ((re-def (pregexp "_\\s+?sc_\\S*?\\(sc\\s*?\\*.*?\\)"))
;;      (re-name (pregexp "sc_\\S*?(?=\\()"))
;;      (ctx "sc"))
;;   (gen))
