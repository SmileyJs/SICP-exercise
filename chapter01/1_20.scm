(load-option 'format)

;The greatest common divisor

(define (gcd x y)
    (format #t "x: ~A y: ~A ~%" x y)
    (if (= y 0)
        x
        (gcd y (remainder x y))
    )
)

(display (gcd 206 40))