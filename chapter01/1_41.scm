;double

(define (double f)
    (lambda (x) (f (f x)))
)

(newline)
(display ((double (lambda (x) (+ x 1))) 5))
(newline)
(display (((double (double double)) (lambda (x) (+ x 1))) 5))