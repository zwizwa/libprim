#lang scheme/base

(require prim-tools)
(parameterize
    ((re-def  (pregexp "_\\s+?px_\\S*?\\(pf\\s*?\\*.*?\\)"))
     (re-name (pregexp "px_\\S*?(?=\\()"))
     (ctx "pf")
     )
  (gen))




