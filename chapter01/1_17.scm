(load-option 'format)

(define (double n)
    (+ n n)
)

(define (halve n)
    (/ n 2)
)

(define (even? x)
    (= 0 (remainder x 2))
)

(define (multi x y)
    (cond ((= x 1) y)
          ((even? x) (multi (halve x) (double y))) 
          (else (+ y (multi (- x 1) y)))
    )
)

(define (multi-iter-helper x y product)
    (format #T "x: ~A, y: ~A, product: ~A ~%" x y product)
    (cond ((= x 0) product)
          ((even? x) (multi-iter-helper (halve x) (double y) product))
          (else (multi-iter-helper (- x 1) y (+ product y)))
    )
)

(define (multi-iter x y)
    (multi-iter-helper x y 0)
)

(newline)
(display (multi 6 6))
(newline)
(display (multi 3 14))
(newline)
(display (multi-iter 3 8))