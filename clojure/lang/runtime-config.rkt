#lang racket/base

(provide configure)

(require (only-in clojure/reader make-clojure-readtable))

(define (configure data)
  (current-readtable (make-clojure-readtable)))

