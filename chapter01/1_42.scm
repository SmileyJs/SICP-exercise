; 1_42

(define (compose f1 f2)
    (lambda (x) (f1 (f2 x)))
)

(define (inc n)
    (+ n 1)
)

(newline)
(display ((compose square inc) 6)) (newline)

; 1_43
(define (repeated f times)
    (if (= times 1)
        f
        (compose f (repeated f (- times 1))))
)

(define (repeated-iter f times)
    (define (iter-helper n result)
        (if (= n 1)
            result
            (iter-helper (- n 1) (compose f result)))
    )

    (iter-helper times f)
)

(display ((repeated-iter square 2) 5)) (newline)

; 1_44
(define dx 0.000001)

(define (smooth f)
    (lambda (x) (/  (+  (f (- x dx))
                        (f x)
                        (f (+ x dx)))
                    3))
)

(define (smooth-times f n)
    ((repeated smooth n) f)
)

(display ((smooth-times square 10) 5))