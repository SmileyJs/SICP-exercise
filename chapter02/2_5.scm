(load "../util.scm")

(define (cons a b)
    (*  (expt 2 a)
        (expt 3 b))
)

(define (car x)
    (if (= 0 (remainder x 2))
        (+ 1 (car (/ x 2)))
        0)
)

(define (cdr x)
    (if (= 0 (remainder x 3))
        (+ 1 (cdr (/ x 3)))
        0)
)

; test
(define cons-x (cons 5 11))

(display cons-x)
(newline)
(display (car cons-x))
(newline)
(display (cdr cons-x))