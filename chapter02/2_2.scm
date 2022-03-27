; 2-2
(load "../util.scm")

(define (make-point x y) (cons x y))
(define (x-point a) (car a))
(define (y-point a) (cdr a))

(define (print-point a)
    (newline)
    (display "(")
    (display (x-point a))
    (display ", ")
    (display (y-point a))
    (display ")")
)

(define (make-segment start end)
    (cons start end)
)

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))


(define (mid-segment s)
    (make-point (average    (x-point (start-segment s))
                            (x-point (end-segment s)))
                (average    (y-point (start-segment s))
                            (y-point (end-segment s))))
)


; test
(define a (make-point 1 2))
(define b (make-point 5 8))

(define t-seg (make-segment a b))
(print-point (start-segment t-seg))
(print-point (end-segment t-seg))
(print-point (mid-segment t-seg))