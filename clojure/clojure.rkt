#lang racket/base

;; Clojure compatibility

(require racket/stxparam
         (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide (except-out (all-from-out racket/base)
                     add1 sub1 if cond #%app quote)
         (rename-out [-#%app #%app]
                     [-quote quote]
                     [sub1 dec]
                     [add1 inc]
                     [clojure:cond cond]
                     [clojure:if if])
         def do let fn defn loop recur
         -> ->>
         partial comp complement constantly
         map true false nil nth)

(define-syntax-parameter recur
  (λ (stx)
    (raise-syntax-error #f "cannot be used outside fn or loop" stx)))

;; basic forms
(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id init)
     #'(define name init)]))

(define-syntax-rule (do expr ...)
  (begin expr ...))

(define true #t)
(define false #f)
;;nil maps most closely to #f
(define nil #f)

;; used for let and loop
(begin-for-syntax
  (define-splicing-syntax-class binding-pair
    #:description "binding pair"
    (pattern (~seq name:id val:expr))))

(define-syntax (let stx)
  (syntax-parse stx
    [(_ (~and binding-list [p:binding-pair ...])
        body:expr ...)
     #:fail-unless (eq? (syntax-property #'binding-list 'paren-shape) #\[)
                   "expected a vector of bindings"
     #'(let* ([p.name p.val] ...)
         body ...)]))

(define-syntax (loop stx)
  (syntax-parse stx
    [(_ (~and binding-list [p:binding-pair ...])
        body:expr ...)
     #:with name #'x
     #:fail-unless (eq? (syntax-property #'binding-list 'paren-shape) #\[)
     "expected a vector of bindings"
     #'(letrec ([name (λ (p.name ...)
                        (syntax-parameterize ([recur (make-rename-transformer #'name)])
                          body ...))])
         (let* ([p.name p.val] ...)
           (syntax-parameterize ([recur (make-rename-transformer #'name)])
             body ...)))]))

(define-syntax (fn stx)
  (syntax-parse stx
    [(_ (~optional name:id #:defaults ([name #'x]))
        [param:id ...] body ...)
     #'(letrec ([name (λ (param ...)
                        (syntax-parameterize ([recur (make-rename-transformer #'name)])
                          body ...))])
         name)]
    [(_ (~optional name:id #:defaults ([name #'x]))
        ([param:id ...] body ...) ...+)
     #'(letrec ([name (syntax-parameterize ([recur (make-rename-transformer #'name)])
                        (case-lambda
                          ([param ...] body ...) ...))])
         name)]))

(define-syntax (defn stx)
  (syntax-parse stx
    [(_ name:id expr ...)
     #'(define name (fn expr ...))]))

;; thrush operators
(define-syntax (-> stx)
  (syntax-parse stx
    [(_ x) #'x]
    [(_ x (e e_1 ...))
     #'(e x e_1 ...)]
    [(_ x e)
     #'(-> x (e))]
    [(_ x form form_1 ...)
     #'(-> (-> x form) form_1 ...)]))

(define-syntax (->> stx)
  (syntax-parse stx
    [(_ x) #'x]
    [(_ x (e e_1 ...))
     #'(e e_1 ... x)]
    [(_ x e)
     #'(->> x (e))]
    [(_ x form form_1 ...)
     #'(->> (->> x form) form_1 ...)]))

(define-syntax (clojure:if stx)
  (syntax-parse stx
    [(_ test then) 
     #'(if test then null)]
    [(_ test then else) 
     #'(if test then else)]))

;; modify lexical syntax via macros
(begin-for-syntax
  ;; check for unquote in these two classes to "noop" commas
  (define-splicing-syntax-class key-value-pair
    (pattern (~seq k:key (~or e:expr (unquote e:expr)))
             #:attr pair #'(k.sym e)))

  (define-syntax-class key
    (pattern (~or e:expr (unquote e:expr))
             #:when (clojure-kwd? #'e)
             #:attr sym #'(quote e)))

  (define-syntax-class vector-literal
    (pattern (~and vec (e:expr ...))
             #:when (eq? (syntax-property #'vec 'paren-shape) #\[)))

  (define (clojure-kwd? e)
    (define exp (syntax-e e))
    (and (symbol? exp)
         (regexp-match #rx":.*" (symbol->string exp)))))

(define-syntax (-quote stx)
  (syntax-parse stx
    ;; quoted vector literals
    [(_ datum:vector-literal)
     #'(vector datum.e ...)]
    ;; Clojure's quote allows multiple arguments
    [(_ e e_1 ...) #'(quote e)]))

(define-syntax (-#%app stx)
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
     #'(#%app proc arg ...)]))

(define-syntax clojure:cond
  (syntax-rules ()
    [(_ :else else-expr)
     (cond (else else-expr))]
    [(_ e1 e2 e3 ... :else else-expr)
     (if (= 0 (modulo (length '(e1 e2 e3 ...)) 2))
         (if e1 e2
             (clojure:cond e3 ... :else else-expr))
         (raise-syntax-error #f "cond requires an even number of forms"))]))

;; lists - examine
;; TODO: use sequences instead
(define nth
  (case-lambda
    [(coll position)
     (cond
      ((list? coll)
       (list-ref coll position))
      ((vector? coll)
       (vector-ref coll position))
      ((string? coll)
       (string-ref coll position)))]
    [(coll position error-msg)
     (cond
      ((list? coll)
       (with-handlers ([exn:fail? (lambda (exn) error-msg)])
         (list-ref coll position)))
      ((vector? coll)
       (with-handlers ([exn:fail? (lambda (exn) error-msg)])
         (vector-ref coll position)))
      ((string? coll)
       (with-handlers ([exn:fail? (lambda (exn) error-msg)])
         (string-ref coll position))))]))

;; useful functions
(require racket/function)

(define partial curry)
(define comp compose)
(define complement negate)
(define constantly const)

;; sequences
(require racket/sequence
         racket/stream)

(define (first s) stream-first)
(define (rest s) stream-rest)
(define (cons fst rst) (stream-cons fst rst))
(define map sequence-map)
