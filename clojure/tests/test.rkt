#lang clojure

(require rackunit)

(displayln [1 2 3])
(check-true (vector? [1 2 3]))
(check-true (vector? '[1 2 3]))
(check-true (immutable? [1 2 3]))
(check-true (immutable? '[1 2 3]))
(displayln {:a 5 :b 7})
(displayln {:a 5, :b 7})
(check-equal? [1,2,3] [1 2 3])
(check-equal? [1 2 (+ 1 2)] [1 2 3])
(check-equal? '[1 2 (+ 1 2)] [1 2 '(+ 1 2)])
(check-equal? [1 2 [3]] (vector-immutable 1 2 (vector-immutable 3)))

(check-pred char? \a)

(def foo 3)
foo

(do 3 5)

(let [x 3 y 5]
  (+ x y))

((fn this [x y] (+ x y)) 5 5)
((fn [x y] (+ x y)) 5 5)
((fn [x] (if (zero? x) 1 (* x (recur (- x 1))))) 5)
((fn ([x] (if (zero? x) 1 (* x (recur (- x 1)))))
     ([x y] (+ (recur x) (recur y))))
 3 2)

(loop [x 3 y 5]
  (+ x y))

;; TODO: make `nil` reader syntax
(check-equal? (if #f 5) nil)

(check-equal?
 (loop [x 5 n 1]
   (if (zero? x)
       n
       (recur (- x 1) (* x n))))
 120)

(defn fact [x]
  (loop [x x n 1]
    (if (zero? x)
        n
        (recur (- x 1) (* x n)))))

(check-equal? (fact 5) 120)

(check-equal? (loop [x 1 y x] y) 1)

;; thrush operators
(require (only-in racket/string string-split string-replace))
(check-equal?
 (-> "a b c d"
     string-upcase
     (string-replace "A" "X")
     (string-split " ")
     car)
 "X")

(check-equal?
 (->> 5 (+ 3) (/ 2) (- 1))
 (/ 3 4))

(check-equal?
  (->> 1 ((fn [x] (+ x 1))))
  2)

;; quote behavior
(check-equal? (quote a b c d) (quote a))
(check-equal? (quote 5 a) 5)
(check-equal? (-> 5 'a) 5)

(check-equal? `(~(+ 1 2)) '(3))

;; if tests based on a post by Jay Fields
(check-equal? "yes" (if true "yes"))
(check-equal? "yes" (if true "yes" "no"))
(check-equal? "no" (if false "yes" "no"))
(check-equal? "no" (if nil "yes" "no"))
(check-equal? "still true" (if -1 "still true" "false"))
(check-equal? "still true" (if 0 "still true" "false"))
(check-equal? "still true" (if [] "still true" "false"))
(check-equal? "still true" (if (list) "still true" "false"))

;; cond tests
(defn factorial [n]
  (cond
   (<= n 1) 1
   :else (* n (factorial (dec n)))))
(check-equal? 120 (factorial 5))

(check-equal? "B" (let [grade 85]
                    (cond
                     (>= grade 90) "A"
                     (>= grade 80) "B"
                     (>= grade 70) "C"
                     (>= grade 60) "D"
                     :else "F")))

(defn pos-neg-or-zero [n]
  (cond
   (< n 0) "negative"
   (> n 0) "positive"
   :else "zero"))
(check-equal? "positive" (pos-neg-or-zero 5))
(check-equal? "negative" (pos-neg-or-zero -1))
(check-equal? "zero" (pos-neg-or-zero 0))

(check-equal? (cond) nil)
(check-equal? (cond false 5) nil)

(check-equal? (nth ["a" "b" "c" "d"] 0) "a")
(check-equal? (nth (list "a" "b" "c" "d") 0) "a")
(check-equal? (nth ["a" "b" "c" "d"] 1) "b")
(check-equal? (nth [] 0 "nothing found") "nothing found")
(check-equal? (nth [0 1 2] 77 1337) 1337)
(check-equal? (nth "Hello" 0) #\H)
(check-equal? (nth '(1 2 3) 0) 1)

(check-equal? (+ 1 2 #_(this is ignored)) 3)

