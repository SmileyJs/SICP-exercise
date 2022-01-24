(load-option 'format)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    ;(format #t "fixed-point ~A ~%" first-guess)
    (define (close-enough? v1 v2)
        ;(format #t "close-enough? ~A ~A ~%" v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (format #t "try value: ~A ~%" guess)
        (let ((next (f guess)))
            (if (close-enough? next guess)
                next
                (try next))))

    (try first-guess)
)

; 1_35
(define golden-ratio
    (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                 1.0))

(display golden-ratio)
(newline)
; 1_36
(define formula
    (lambda (x) (/ (log 1000) (log x)))
)

(define (average-damp f)
    (lambda (x) (/ (+ x (f x)) 2))
)

(display (fixed-point formula 2.0))
(newline)
(display (fixed-point (average-damp formula) 2.0))