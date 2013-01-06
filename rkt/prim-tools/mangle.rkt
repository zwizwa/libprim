#lang scheme/base
(provide scheme->c
         c->scheme)

(define (pre->suf pre x __suf)
  (if (regexp-match pre x)
      (string-append (regexp-replace pre x "") __suf)
      x))
(define (pre->pre pre x pre__)
  (if (regexp-match pre x)
      (string-append pre__ (regexp-replace pre x ""))
      x))
(define (suf->pre suf x pre__)
  (if (regexp-match suf x)
      (string-append pre__ (regexp-replace suf x ""))
      x))

;; FIXME: use a declarative specification and derive forward and
;; backward transforms.

(define (c->scheme x)
  (let* ((x (regexp-replace  #px"^\\S*?_" x ""))
         (x (regexp-replace* #px"_"       x "-"))
         (x (regexp-replace* #px"-to-"    x "->"))
         (x (regexp-replace* #px"-with-"  x "/"))
         (x (pre->suf        #px"^bang-"  x "!"))
         (x (pre->suf        #px"^fetch-" x "@"))
         (x (pre->suf        #px"^from-"  x ">"))
         (x (pre->suf        #px"^is-"    x "?"))
         (x (pre->pre        #px"^to-"    x ">"))
         )x))

(define (scheme->c x [prefix "ex_"])
  (let* ((x (pre->pre        #px"^>"   x "to-"))
         (x (suf->pre        #px"\\?$" x "is-"))
         (x (suf->pre        #px">$"   x "from-"))
         (x (suf->pre        #px"@$"   x "fetch-"))
         (x (suf->pre        #px"!$"   x "bang-"))
         (x (regexp-replace* #px"/"    x "-with-"))
         (x (regexp-replace* #px"->"   x "-to-"))
         (x (regexp-replace* #px"-"    x "_"))
         ) (string-append prefix x)))

