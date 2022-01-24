; calulate the cube of x

(define (average x y)
    (/ (+ x y) 2)
)

(define (square x)
    (* x x)
)

(define (good-enough? old new)
    (>  0.001
        (/  (abs (- new old))
            old
        )
    )
)

(define (improve guess x)
    (display "improve guess:\n ")
    (/  (+  (/ x (square guess))
            (* guess 2))
        3))

(define (cube-iter guess x)
    (if (good-enough? guess (improve guess x))
        guess
        (cube-iter (improve guess x) x)    
    )
)

(define (cube x)
    (cube-iter 1.0 x)
)

(write (cube 27))