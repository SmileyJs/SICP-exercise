(load "./2_36.scm")

(define list-a (list 1 2 3 4))
(define list-b (list 5 6 7 8))
(define list-c (list 9 10 11 12))
(define list-v (list 1 1 1 1))

(define matrix-a (list list-a list-b list-c))

(define (dot-product v w)
    (accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
    (map (lambda (w) (dot-product v w))
         m)
)

(define (transpose mat)
    (accumulate-n cons () mat)
)

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (row-m) (matrix-*-vector cols row-m))
             m))
)

(display matrix-a)
(newline)
(display (map + list-a list-b list-c))
(newline)
(display (dot-product list-a list-b))
(newline)
(display (matrix-*-vector matrix-a list-v))
(newline)
(display (transpose matrix-a))
(newline)
(display (matrix-*-matrix matrix-a (transpose matrix-a)))