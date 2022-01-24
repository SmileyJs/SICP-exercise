; fibonacci

(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2))))
    )

)

(define (fib_iter_helper a b count)
    (if (= count 0)
        b
        (fib_iter_helper (+ a b) a (- count 1))
    )
)

(define (fib_iter n)
    (fib_iter_helper 1 0 n)
)


(write (fib 7))
(write (fib_iter 7))