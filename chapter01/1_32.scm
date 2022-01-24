;1-32

(load-option 'format)

(define (accumulate combiner null-value term a next b)
    ;(format #t "accumulate a: ~A, b: ~A ~%" a b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))
    )
)

(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))
        ))

    (iter a null-value)
)

(define (sum a b)
    ;(format #t "sum a: ~A, b: ~A ~%" a b)
    (accumulate-iter +
                0
                (lambda (x) x)
                a
                (lambda (x) (+ x 1))
                b)
)

(define (product a b)
    (accumulate-iter *
                1
                (lambda (x) x)
                a
                (lambda (x) (+ x 1))
                b)
)

(display (sum 1 10))
(newline)
(display (product 1 10))

