#lang racket/base

(provide nil nil?)

(define nil (void))

(define (nil? v) (void? v))

