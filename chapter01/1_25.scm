(load "1_24.scm")

(define (fast-expt b n)
    (if (= n 0) 1
        (if (even? n) (square (fast-expt b (/ n 2))) 
            (* b (fast-expt b (- n 1))) 
        )
    )
)

; 1-25, override 1-24's expmod
(define (expmod base exp m)
    ;(format #t "base: ~A, exp: ~A, m: ~A ~%" base exp m)
    (remainder (fast-expt base exp) m)
)

(search-for-prime 1000 30)
(search-for-prime 10000 30)
(search-for-prime 100000 30)
(search-for-prime 1000000 30)
