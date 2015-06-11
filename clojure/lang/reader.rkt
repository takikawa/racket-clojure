#lang s-exp syntax/module-reader
clojure/clojure
#:language-info '#[clojure/lang/language-info get-language-info #f]
#:wrapper1
(lambda (t)
  (parameterize ([current-readtable (make-clojure-readtable)])
    (t)))

(require clojure/reader)

