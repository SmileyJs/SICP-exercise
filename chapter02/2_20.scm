(load "../util.scm")

(define (same-parity x . rest)
    (filter (if (even? x)
                even?
                odd?)
            (cons x rest))
)

(define (same-parity-2 x . rest)
    (define f (if (even? x)
                    even?
                    odd?))

    (define (iter items result)
        (if (null? items)
            result
            (iter (cdr items)
                (if (f (car items))
                    (append result (list (car items)))
                    result))))

    (iter rest '())
)

(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity-2 2 3 4 5 6 7))