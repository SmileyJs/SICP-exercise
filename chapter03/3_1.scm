(load "../util.scm")


(define (make-accumulator n)
    (lambda (m)
        (set! n (+ n m))
        n
    )
)

(define A (make-accumulator 10))

(display (A 14))
(newline)
(display (A 3))
