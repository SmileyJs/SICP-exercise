; 1_24 fermat 

(load-option 'format)

(define (expmod base exp m)
    ;(format #t "base: ~A, exp: ~A, m: ~A ~%" base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m)) m))
          (else
            (remainder (* base (expmod base (- exp 1) m)) m)
          )
    )
)

(define (fermat-test n)
    ;(format #t "fermat-test ~%")
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
    ;(format #t "fast-prime? ~%")
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false))
)

(define (next-odd n)
    ;(format #t "next-odd")
    (if (odd? n)
        (+ n 2)
        (+ n 1))
)

(define (continue-prime n count)
    ;(format #t "fast-prime? ~%")
    (cond ((= count 0) 
            (display "are primes\n"))
          ((fast-prime? n 3)
            (display n)
            (newline)
            (continue-prime (next-odd n) (- count 1)))
          (else
            (continue-prime (next-odd n) count))
    )
)

(define (search-for-prime start-number count)
    (let ((start-time (real-time-clock)))
        (continue-prime start-number count)
        (format #t "time-cost: ~A ~%" (- (real-time-clock) start-time)))
)

(search-for-prime 1000 30)
;(search-for-prime 10000 3)
;(search-for-prime 100000 3)
;(search-for-prime 1000000 3)