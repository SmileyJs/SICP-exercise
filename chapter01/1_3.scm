; To return the sum of two beggest numbers among 3 input numbers

; Function to calculate the sum of two numbers
(define (sum a b)
    (+ a b)
)

(define (bigger a b)
    (if (> a b)
        a
        b
    )
)

(define (smaller a b)
    (if (> a b)
        b
        a
    )
)

(define (sum_of_bigger_two a b c)
    (sum (bigger a b) (bigger c (smaller a b)))
)

(write (sum_of_bigger_two 5 6 7))