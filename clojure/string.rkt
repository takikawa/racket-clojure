#lang racket/base

;; String functions

(module+ test (require rackunit))

(define upper-case string-upcase)
(define lower-case string-downcase)

;; String -> String
;; capitalize the first character and down the rest
(define (capitalize str)
  (cond [(= (string-length str) 0)
         str]
        [(= (string-length str) 1)
         (string-upcase str)]
        [(> (string-length str) 1)
         (string-append (string-upcase (substring str 0 1))
                        (string-downcase (substring str 1)))]))

(module+ test
  (check-equal? (capitalize "") "")
  (check-equal? (capitalize "a") "A")
  (check-equal? (capitalize "MiXeD cAsE") "Mixed case")
  (check-equal? (capitalize "mIxEd CaSe") "Mixed case"))

