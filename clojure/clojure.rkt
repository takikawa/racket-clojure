#lang racket/base

;; Clojure compatibility

(require (prefix-in rkt: racket/base)
         (prefix-in rkt: racket/set)
         racket/stxparam
         "nil.rkt"
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     syntax/strip-context
                     ))

(provide (except-out (all-from-out racket/base)
                     add1 sub1 if cond #%app #%datum #%top quote)
         (rename-out [-#%app #%app]
                     [-#%datum #%datum]
                     [-#%top #%top]
                     [-quote quote]
                     [sub1 dec]
                     [add1 inc]
                     [clojure:cond cond]
                     [clojure:if if])
         def do let fn defn loop recur
         -> ->>
         partial comp complement constantly
         vector str
         hash-map map?
         hash-set set?
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

;; used for let and loop
(begin-for-syntax
  (define-splicing-syntax-class binding-pair
    #:description "binding pair"
    (pattern (~seq name:id val:expr))))

(define-syntax (let stx)
  (syntax-parse stx
    [(_ #[p:binding-pair ...]
        body:expr ...)
     #'(let* ([p.name p.val] ...)
         body ...)]))

(define-syntax (loop stx)
  (syntax-parse stx
    [(_ #[p:binding-pair ...]
        body:expr ...)
     #:with name #'x
     #'(letrec ([name (λ (p.name ...)
                        (syntax-parameterize ([recur (make-rename-transformer #'name)])
                          body ...))])
         (let* ([p.name p.val] ...)
           (name p.name ...)))]))

(define-syntax (fn stx)
  (syntax-parse stx
    [(_ (~optional name:id #:defaults ([name #'x]))
        #[param:id ...] body ...)
     #'(letrec ([name (λ (param ...)
                        (syntax-parameterize ([recur (make-rename-transformer #'name)])
                          body ...))])
         name)]
    [(_ (~optional name:id #:defaults ([name #'x]))
        (#[param:id ...] body ...) ...+)
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

(define (true? v)
  (not (or (eq? v #f) (eq? v nil))))

(define-syntax (clojure:if stx)
  (syntax-parse stx
    [(_ test then) 
     #'(if (true? test) then nil)]
    [(_ test then else) 
     #'(if (true? test) then else)]))

;; modify lexical syntax via macros
(begin-for-syntax
  (define-splicing-syntax-class key-value-pair
    (pattern (~seq k:key e:expr)
             #:attr pair #'(k.sym e)))

  (define-syntax-class key
    (pattern e:expr
             #:when (clojure-kwd? #'e)
             #:attr sym #'(quote e)))

  (define (clojure-kwd? e)
    (define exp (syntax-e e))
    (and (symbol? exp)
         (regexp-match #rx":.*" (symbol->string exp)))))

(define-syntax (-quote stx)
  (syntax-parse stx
    ;; Clojure's quote allows multiple arguments
    [(_ e e_1 ...) #'(quote e)]))

(define-syntax -#%datum
  (lambda (stx)
    (syntax-parse stx
      [(-#%datum . #[e ...])
       (syntax/loc stx (vector e ...))]
      [(-#%datum . hsh)
       #:when (syntax-property #'hsh 'clojure-hash-map)
       #:with (e ...) (replace-context #'hsh (syntax-property #'hsh 'clojure-hash-map))
       (syntax/loc stx (hash-map e ...))]
      [(-#%datum . st)
       #:when (syntax-property #'st 'clojure-set)
       #:with (e:expr ...) (replace-context #'st (syntax-property #'st 'clojure-set))
       (syntax/loc stx (hash-set e ...))]
      [(-#%datum . e)
       (syntax/loc stx (#%datum . e))])))

(define-syntax (-#%app stx)
  (syntax-parse stx
    [(_ proc:expr arg:expr ...)
     #'(#%app proc arg ...)]))

(define-syntax -#%top
  (lambda (stx)
    (syntax-parse stx
      [(-#%top . id)
       #:when (syntax-property #'id 'clojure-keyword)
       (syntax/loc stx (quote id))]
      [(-#%top . id)
       (syntax/loc stx (#%top . id))])))

(define-syntax clojure:cond
  (lambda (stx)
    (syntax-case stx (:else)
      [(_)
       #'nil]
      [(_ :else else-expr)
       #'else-expr]
      [(_ e1 e2 e3 ...)
       (if (even? (length (syntax->list #'(e1 e2 e3 ...))))
           #'(if (true? e1) e2
                 (clojure:cond e3 ...))
           (raise-syntax-error #f "cond requires an even number of forms" stx))])))

;; lists - examine
(define nth
  (case-lambda
    [(coll position)
     (sequence-ref coll position)]
    [(coll position error-msg)
     (if (or (= 0 (sequence-length coll))
	     (> position (sequence-length coll)))
	 error-msg
	 (sequence-ref coll position))]))

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

(define (vector . args)
  (apply vector-immutable args))

(define (str . args)
  (string->immutable-string
   (apply string-append (rkt:map toString args))))

;; private: can return a mutable string because str will still produce an immutable one
(define (toString v)
  (cond [(rkt:string? v) v]
        [(nil? v) ""]
        [(char? v) (rkt:string v)]
        [else (format "~s" v)])) ;; FIXME implement clojure printer and pr-str

(define (hash-map . args)
  (apply hash args))

(define (map? v)
  (and (hash? v) (immutable? v)))

(define (hash-set . args)
  (apply rkt:set args))

(define (set? v)
  (rkt:set? v))

