#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide (except-out (all-from-out racket/base) #%app)
         (rename-out [-#%app #%app]))

(begin-for-syntax
  (define (clojure-kwd? e)
    (define exp (syntax-e e))
    (and (symbol? exp)
         (regexp-match #rx":.*" (symbol->string exp)))))

(define-syntax (-#%app stx)

  ;; check for unquote in these two classes to "noop" commas
  (define-splicing-syntax-class key-value-pair
    (pattern (~seq k:key (~or e:expr (unquote e:expr)))
             #:attr pair #'(k.sym e)))

  (define-syntax-class key
    (pattern (~or e:expr (unquote e:expr))
             #:when (clojure-kwd? #'e)
             #:attr sym #'(quote e)))

  (syntax-parse stx
    ;; [1 2 3] is an array
    [(_ e:expr ...)
     #:when (eq? (syntax-property stx 'paren-shape) #\[)
     #'(vector e ...)]
    ;; {:a 1 :b 2} is a hash
    ;; {:a 1, :b 3} is too
    [(_ kv:key-value-pair ...)
     #:when (eq? (syntax-property stx 'paren-shape) #\{)
     #:with (key-vals ...)
            (datum->syntax
              #'(kv ...)
              (apply append (syntax->datum #'(kv.pair ...))))
     #'(hash key-vals ...)]
    [(_ proc:expr arg:expr ...)
     #'(proc arg ...)]))
