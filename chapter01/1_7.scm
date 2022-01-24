; calulate the sqrt of x

(define (new-if predicate then-clause else-clause)
    (cond (predicate hen-clause)
          (else else-clause)
    )
)

(define (average x y)
    (/ (+ x y) 2)
)

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001)
)

(define (good-enough-new? old new)
    (>  0.001
        (/  (abs (- new old))
            old
        )
    )
)

(define (improve guess x)
    (display "improve guess:\n ")
    (average (/ x guess) guess)
)

(define (sqrt-iter guess x)
    (if (good-enough-new? guess (improve guess x))
        guess
        (sqrt-iter (improve guess x) x)    
    )
)

(define (sqrt x)
    (sqrt-iter 1.0 x)
)

(write (sqrt 9000000000000000000000000000000000000000000000000000000009000000000000000000000000000000000000000000000000))