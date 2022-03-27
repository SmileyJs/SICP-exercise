; utility method the SICP execrises.

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))
)

(define (average a b)
    (/  (+ a b)
        2)
)

(define (expt b n)
    (define (expt-iter base x product)
        (if (= 0 x)
            product
            (if (even? x)
                (expt-iter (square base) (/ x 2) product)
                (expt-iter base (- x 1) (* product base)))))
    
    (expt-iter b n 1)
)

(define nil '())