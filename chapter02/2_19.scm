(load "../util.scm")
(load "2_17.scm")

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? items)
    (null? items)
)

(define (first-denomination coin-values)
    (car coin-values)
)

(define (except-first-denomination coin-values)
    (cdr coin-values)
)

(define (cc amount coin-values)
    (cond ((= 0 amount) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+  (cc amount (except-first-denomination coin-values))
                (cc (- amount (first-denomination coin-values)) coin-values))))
)

(display (cc 100 us-coins))