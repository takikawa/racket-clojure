#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide (except-out (all-from-out racket/base)
                     #%app)
         (rename-out [-#%app #%app])
         def do let fn)

;; basic forms
(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id init)
     #'(define name init)]))

(define-syntax-rule (do expr ...)
  (begin expr ...))

(define-syntax (let stx)
  (define-splicing-syntax-class binding-pair
    #:description "binding pair"
    (pattern (~seq name:id val:expr)))
  
  (syntax-parse stx
    [(_ (~and binding-list [p:binding-pair ...])
        body:expr ...)
     #:fail-unless (eq? (syntax-property #'binding-list 'paren-shape) #\[)
                   "expected a vector of bindings"
     #'(let* ([p.name p.val] ...)
         body ...)]))

(define-syntax (fn stx)
  (syntax-parse stx
    [(_ name:id [param:id ...] body ...)
     #'(letrec ([name (λ (param ...) body ...)])
         name)]
    [(_ name:id ([param:id ...] body ...) ...+)
     #'(letrec ([name (case-lambda
                        ([param ...] body ...) ...)])
         name)]
    [(_ [param:id ...] body ...)
     #'(λ (param ...) body ...)]
    [(_ ([param:id ...] body ...) ...+)
     #'(case-lambda ([param ...] body ...) ...)]))

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
