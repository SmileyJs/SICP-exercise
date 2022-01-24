(define (func n)
    (if (< n 3)
        n
        (+ (func (- n 1)) (* 2 (func (- n 2))) (* 3 (func (- n 3))))
    )
)

(define (func_iter a b c index n)
    (if (= index n)
        a
        (func_iter b c (+ (* a 3) (* b 2) c) (+ index 1) n)
    )
)

(define (func_iter_start n)
    (func_iter 0 1 2 0 n)
)

(write (func 10))
(write (func_iter_start 10))