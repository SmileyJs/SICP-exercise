; Miller-rabin prime check

(load "1_24.scm")

(define (expmod base exp m)
    ;(format #t "base: ~A, exp: ~A, m: ~A ~%" base exp m)
    (cond ((= exp 0) 1)
          ((not-trivial-square-root base m)
            0)
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m)) m))
          (else
            (remainder (* base (expmod base (- exp 1) m)) m))
    )
)

(define (not-trivial-square-root a n)
    (and (not (= a 1))
         (not (= (- n 1) a))
         (= 1 (remainder (square a) n))
    )
)

(define (test-iter n times)
    (define (try-it a)
        (= (expmod a (- n 1) n) 1))

    (cond ((= times 0) true)
          ((try-it (+ 1 (random (- n 1)))) (test-iter n (- times 1)))
          (else false))
)

(define (Miller-Rabin-test n)
    (let ((times (ceiling (/ n 2))))
        (test-iter n times))
)

(define (Miller-Rabin-test2 n)
    (define (try-it a)
        (= (expmod a (- n 1) n) 1))
    (try-it (+ 1 (random (- n 1))))
)

(define (test-result n)
    (if (Miller-Rabin-test n)
        (format #t "~A is prime~%" n)
        (format #t "~A is not prime~%" n)    
    )
)

(test-result 1105)
(test-result 1000)
(test-result 1729)
(test-result 1009)