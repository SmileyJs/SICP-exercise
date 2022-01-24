; iterative-improve

(load-option 'format)

(define (iterative-improve good-enough? improve)
    (lambda (first-guess)
        (define (try guess)
            (let ((next (improve guess)))
                (if (good-enough? guess next)
                    next
                    (try next))))
        (try first-guess)
    )
)

(define tolerance 0.00001)

(define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
)

(define (fixed-point f first-guess)
    ((iterative-improve close-enough? f) first-guess)
)

(newline)
(display (fixed-point cos 1.0)) (newline)

(define (average x y)
    (format #t "average x:~A, y:~A ~%" x y)
    (/ (+ x y) 2)
)

(define (sqrt num)
    (define (improve guess)
        (average guess (/ num guess))
    )

    ((iterative-improve close-enough? improve) 1.0)
)

(display (sqrt (exact->inexact 9))) (newline)