#lang clojure

(require rackunit)

(displayln [1 2 3])
(check-true (vector? [1 2 3]))
(check-true (vector? '[1 2 3]))
(displayln {:a 5 :b 7})
(displayln {:a 5, :b 7})

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

;; quote behavior
(check-equal? (quote a b c d) (quote a))
(check-equal? (quote 5 a) 5)
(check-equal? (-> 5 'a) 5)

;if tests based on a post by Jay Fields
(check-equal? "yes" (if true "yes"))
(check-equal? "yes" (if true "yes" "no"))
(check-equal? "no" (if false "yes" "no"))
(check-equal? "no" (if nil "yes" "no"))
(check-equal? "still true" (if -1 "still true" "false"))
(check-equal? "still true" (if 0 "still true" "false"))
(check-equal? "still true" (if [] "still true" "false"))
(check-equal? "still true" (if (list) "still true" "false"))

;cond tests
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
