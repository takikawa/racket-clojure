#lang racket/base

(provide apropos)

;; (U String Regexp) -> (List Symbol)
;; lookup names in the current namespace
(define (apropos search-term)
  (filter (λ (sym)
            (define str (symbol->string sym))
            (if (regexp? search-term)
                (regexp-match search-term str)
                (equal? search-term str)))
          (namespace-mapped-symbols)))

