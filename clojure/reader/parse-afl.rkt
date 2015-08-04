#lang racket/base

(provide parse-afl current-syntax-introducer make-intro)

(require racket/match
         racket/list
         syntax/srcloc
         syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     ))
(module+ test
  (require rackunit))

(define-simple-macro (require-a-lot req-spec ...)
  #:with (phase ...) (range -10 11)
  (require (for-meta phase req-spec ...) ...))

(require-a-lot (only-in racket/base lambda define-syntax #%app make-rename-transformer syntax))

(define current-syntax-introducer (make-parameter (λ (x) x)))

(define make-intro
  (cond [(procedure-arity-includes? make-syntax-introducer 1)
         (λ () (make-syntax-introducer #t))]
        [else
         (λ () (make-syntax-introducer))]))

(define (parse-afl stx)
  (define intro (current-syntax-introducer))
  (define stx* (intro stx))
  (with-syntax ([args (parse-args stx*)]
                [% (datum->syntax stx* '%)]
                [%1 (datum->syntax stx* '%1)]
                [body stx*])
    (intro
     (syntax/loc stx
       (lambda args
         (define-syntax % (make-rename-transformer #'%1))
         body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define chk (compose1 syntax->datum parse-afl))
  (check-equal? (chk #'(+))
                '(lambda ()
                  (define-syntax % (make-rename-transformer #'%1))
                  (+)))
  (check-equal? (chk #'(+ 2 %1 %1))
                '(lambda (%1)
                  (define-syntax % (make-rename-transformer #'%1))
                  (+ 2 %1 %1)))
  (check-equal? (chk #'(+ 2 %3 %2 %1))
                '(lambda (%1 %2 %3)
                  (define-syntax % (make-rename-transformer #'%1))
                  (+ 2 %3 %2 %1)))
  (check-equal? (chk #'(apply list* % %&))
                '(lambda (%1 . %&)
                  (define-syntax % (make-rename-transformer #'%1))
                  (apply list* % %&)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse-args : Stx -> Formals-Stx
(define (parse-args stx)
  ;; Filter the stxs to those that start with %, 
  ;; find the maximum, find whether there is a
  ;; rest argument, and produce lambda formals
  ;; based on that.
  (define-values (max-num rest?)
    (find-arg-info stx))
  (define datum-formals
    (append (for/list ([n (in-range 1 (add1 max-num))])
              (string->symbol (format "%~v" n)))
            (cond [rest? '%&]
                  [else '()])))
  (datum->syntax stx datum-formals stx))

;; find-arg-info : Any -> (Values Natural Boolean)
(define (find-arg-info v)
  (match (maybe-syntax-e v)
    [(? symbol? sym) (find-arg-info/sym sym)]
    [(? pair? pair)  (find-arg-info/pair pair)]
    [_               (return)]))

;; find-arg-info/sym : Symbol -> (Values Natural Boolean)
(define (find-arg-info/sym sym)
  (define str (symbol->string sym))
  (match str
    ["%"  (return #:max-num 1)]
    ["%&" (return #:rest? #t)]
    [(regexp #px"^%\\d$")
     (return #:max-num (string->number (substring str 1)))]
    [_ (return)]))

;; find-arg-info/pair :
;;   (Cons Symbol Symbol) -> (Values Natural Boolean)
(define (find-arg-info/pair pair)
  (define-values (car.max-num car.rest?)
    (find-arg-info (car pair)))
  (define-values (cdr.max-num cdr.rest?)
    (find-arg-info (cdr pair)))
  (return #:max-num (max car.max-num cdr.max-num)
          #:rest? (or car.rest? cdr.rest?)))

(define (return #:max-num [max-num 0] #:rest? [rest? #f])
  (values max-num rest?))

(define (maybe-syntax-e stx)
  (cond [(syntax? stx) (syntax-e stx)]
        [else stx]))
