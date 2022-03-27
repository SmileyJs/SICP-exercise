(load "../util.scm")

(define (cons x y)
    (lambda (f) (f x y))
)

(define (car z)
    (z (lambda (a b) a))
)

(define (cdr z)
    (z (lambda (a b) b))
)

; test
(define cons-a (cons 2 4))

(display (car cons-a))
(newline)
(display (cdr cons-a))