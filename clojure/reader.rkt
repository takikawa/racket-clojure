#lang racket/base

(provide make-clojure-readtable)

(require racket/port)

(define (make-clojure-readtable [rt (current-readtable)])
  (make-readtable rt
                  #\~ #\, #f
                  #\, #\space #f
                  #\_ 'dispatch-macro s-exp-comment-proc
                  #\[ 'terminating-macro vec-proc
                  #\\ 'non-terminating-macro char-proc
                  ))

(define (s-exp-comment-proc ch in src ln col pos)
  (make-special-comment (read-syntax/recursive src in)))

(define (vec-proc ch in src ln col pos)
  (define lst-stx
    (parameterize ([read-accept-dot #f])
      (read-syntax/recursive src in ch (make-readtable (current-readtable) ch #\[ #f))))
  (define lst (syntax->list lst-stx))
  (datum->syntax lst-stx (list->immutable-vector lst) lst-stx lst-stx))

(define (list->immutable-vector lst)
  (apply vector-immutable lst))

(define (char-proc ch in src ln col pos)
  (define in*
    (parameterize ([port-count-lines-enabled #t])
      (input-port-append #f (open-input-string "\\") in)))
  (set-port-next-location! in* ln col pos)
  (read-syntax/recursive src in* #\# #f))

