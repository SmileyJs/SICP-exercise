
(define (dec a)
    (- a 1)
)

(define (inc a)
    (+ a 1)
)

(define (plus_1 a b)
    (if (= a 0)
        b
        (inc (plus_1 (dec a) b))
    )
)

(define (plus_2 a b)
    (if (= a 0)
        b
        (plus_2 (dec a) (inc b))
    )
)

(write (plus_1 4 5))

(write (plus_2 4 5))