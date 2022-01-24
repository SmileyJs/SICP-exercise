(define (expt b n)
    (expt-iter b n 1)
)

(define (expt-iter b n product)
    (if (= n 0) product
        (expt-iter b (- n 1) (* product b))
    )
)

(define (even? n)
    (= (remainder n 2) 0)
)

(define (fast-expt b n)
    (if (= n 0) 1
        (if (even? n) (square (fast-expt b (/ n 2))) 
            (* b (fast-expt b (- n 1))) 
        )
    )
)

(define (fast-expt-iter b n)
    (iter-helper b n 1)
)

(define (iter-helper b n product)
    (if (= n 0) product
        (if (even? n) (iter-helper (square b) (/ n 2) product)
            (iter-helper b (- n 1) (* product b))
        )
    )
)

(write (expt 2 4))
(write (fast-expt 2 5))
(write (fast-expt-iter 2 6))
