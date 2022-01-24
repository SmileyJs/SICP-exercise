; smallest-divisor

(load-option 'format)

(define (smallest-divisor n)
    (find-divisor n 2)
)

(define (next n)
    (+ n 1)
)

(define (find-divisor n test-divisor)
    ;(format #T "n: ~A, test-divisor: ~A ~%" n test-divisor)
        ; n < test-divisor, is prime, return n
    (cond ((> (square test-divisor) n) n)
        ; n / test-divisor == 0, return test-divisor
        ((divides? test-divisor n) test-divisor)
        ; test-divisor++, call find-divisor
        (else (find-divisor n (next test-divisor)))
    )
)

(define (divides? x y)
    (= 0 (remainder y x))
)

(define (prime? n)
    (= n (smallest-divisor n))
)

; 1-21
(define (test-prime n)
    (if (prime? n) 
        (format #T "~A isPrime~%" n)
        (format #T "~A isNotPrime~%" n))
)

(test-prime 199)
(test-prime 1999)
(test-prime 19999)

; 1-22
(define (timed-prime-test n)
    (format #T "testing: ~A ... ~%" n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime-time (- (runtime) start-time)))
)

(define (report-prime-time elapsed-time)
    (format #T "*** time: ~A~%" elapsed-time)
)

(timed-prime-test 1999)

(define (next-odd n)
    (if (odd? n)
        (+ n 2)
        (+ n 1))
)

(define (continue-prime n count)
    (cond ((= count 0) 
            (display "are primes\n"))
          ((prime? n)
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
        (format #T "time-cost: ~A ~%" (- (real-time-clock) start-time)))
)

(search-for-prime 1000 30)
(search-for-prime 10000 30)
(search-for-prime 100000 30)
(search-for-prime 1000000 30)