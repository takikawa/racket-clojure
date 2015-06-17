#lang racket/base

(provide configure)

(require (only-in clojure/reader make-clojure-readtable)
         (only-in clojure/printer pr))

(define (configure data)
  (current-readtable (make-clojure-readtable))
  (current-print (make-print-proc (current-print)))
  )

(struct clojure-pr-thing (v)
  #:property prop:custom-write
  (λ (this out mode)
    (pr (clojure-pr-thing-v this) #:out out)))

(define ((make-print-proc orig-print-proc) v)
  (cond [(void? v) v]
        [else (orig-print-proc (clojure-pr-thing v))]))

