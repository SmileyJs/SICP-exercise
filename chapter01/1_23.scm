(load "1_21.scm")

(define (next n)
    (if (= n 2)
        3
        (+ n 2))
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


(search-for-prime 1000 30)
(search-for-prime 10000 30)
(search-for-prime 100000 30)
(search-for-prime 1000000 30)