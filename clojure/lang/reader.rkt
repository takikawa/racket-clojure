#lang racket/base

(provide read read-syntax get-info)

(require clojure/reader
         (prefix-in - "reader-no-wrap.rkt"))

(define read (wrap-reader -read))
(define read-syntax (wrap-reader -read-syntax))
(define get-info -get-info)

