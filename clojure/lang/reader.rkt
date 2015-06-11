#lang s-exp syntax/module-reader
clojure/clojure
#:wrapper1
(lambda (t)
  (parameterize ([current-readtable (make-clojure-readtable)])
    (t)))
(define (make-clojure-readtable [rt (current-readtable)])
  (make-readtable rt
                  #\~ #\, #f
                  #\, #\space #f
                  #\_ 'dispatch-macro s-exp-comment-proc
                  ))

(define (s-exp-comment-proc ch in src ln col pos)
  (make-special-comment (read-syntax/recursive src in)))

