(load-option 'format)

(define (even? x)
    (= 0 (remainder x 2))
)

;p' = p^2 + q^2 ; q' = 2pq + q^2

(define (fib-log-helper a b p q count)
    (format #T "a: ~A, b: ~A, p: ~A, q: ~A, count: ~A ~%" a b p q count)
    (cond ((= count 0) b)
          ((even? count) 
            (fib-log-helper a
                            b
                            (+ (square p) (square q))
                            (+ (square q) (* 2 p q))
                            (/ count 2)))
          (else (fib-log-helper (+ (* b q) (* a q) (* a p))
                                (+ (* b p) (* a q))
                                p
                                q
                                (- count 1)))
    )
)

(define (fib-log n)
    (fib-log-helper 1 0 0 1 n)
)

(display (fib-log 15))