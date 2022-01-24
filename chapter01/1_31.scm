; 1_31.scm

(load-option 'format)

(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b)))
)

(define (product-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* a result)))
    )

    (iter a 1)
)

(define (inc n)
    (+ n 1)
)

(define (identity n)
    n
)

(define (product-integers a b)
    (product-iter identity a inc b)
)

(define (factorial n)
    (product (lambda (x) x)
             1
             (lambda (x) (+ x 1))
             n)
)

(display (product-integers 1 10))
(newline)
(display (factorial 10))
(newline)
(display (exact->inexact (/ 5 2)))
(newline)

(define (numer i)
    (if (odd? i)
        (+ i 1)
        (+ i 2))
)

(define (denom i)
    (if (odd? i)
        (+ i 2)
        (+ i 1))
)

(define (Pi n)
    (let ((start-time (real-time-clock)))
        (format #t "value: ~A ~%"
            (*  4
                (exact->inexact
                    (/  (product-iter numer 1 inc n)
                        (product-iter denom 1 inc n)
                    ))))
        (format #t "time cost: ~A ~%"
            (- (real-time-clock) start-time)))
)

(Pi 1000)
(Pi 10000)
(Pi 100000)