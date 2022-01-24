;

(load-option 'format)

(define (cont-frac f-n f-d k)
    (define (frac-item i)
        (if (= i k)
            (/ (f-n i) (f-d i))
            (/ (f-n i) (+ (f-d i) (frac-item (+ i 1)))))
    )

    (frac-item 1)
)

(define (cont-frac-iter f-n f-d k)
    (define (iter-helper i result)
        (if (> i 0)
            (iter-helper (- i 1) (/ (f-n i) (+ (f-d i) result)))
            result))

    (iter-helper k 0)
)

(display (cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 11))
(newline)
(display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) 11))
(newline)
;1_38

(define (n-generator i)
    (if (= 0 (remainder (+ i 1) 3))
        (/ (* 2 (+ i 1)) 3)
        1)
)

(display (cont-frac (lambda (x) 1.0) n-generator 10))
(newline)

;1_39
(define (tan-cf x k)
    (define (f-x i)
        (if (= i 1)
            x
            (- (square x))))

    (define (f-d i)
        (- (* 2 i) 1))

    (exact->inexact (cont-frac f-x f-d k))
)

(display (tan-cf 10 100))