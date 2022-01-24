; 1_45.scm
(load-option 'format)

(load "1_42.scm")

(define (expt x n)
    (define (iter-helper base i result)
        ;(format #t "expt-iter-helper, base: ~A, i: ~A, result: ~A ~%" base i result)
        (cond   ((= i 0) result)
                ((even? i) (iter-helper (square base) (/ i 2) result))
                (else (iter-helper base (- i 1) (* base result)))))

    (iter-helper x n 1)
)

(define (expt-repeated base n)
    ((repeated-iter (lambda (x) (* x base)) n) 1)
)

(display (expt 2 10)) (newline)
(display (expt-repeated 2 4)) (newline)

(define (formula x n)
    (lambda (y) (/ x (expt y (- n 1))))
)

(define (average-damp f)
    (lambda (x) 
        ;(format #t "average-damp x: ~A ~%" x)
        (/ (+ x (f x)) 2))
)

(define (average-damp-times f n)
    ((repeated-iter average-damp n) f)
)

(display ((average-damp-times square 10) 10.0)) (newline)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? next guess)
                next
                (try next))))

    (try first-guess)
)

(define (damp-nth-root n damp-times)
    (lambda (x)
        (fixed-point (average-damp-times (formula x n) damp-times) 1.0))
)

(display ((damp-nth-root 8 3) 256)) (newline)

; the damp-times should be large than log2(n-root)

(define (log n)
    (cond   ((> (/ n 2) 1) (+ 1 (log (/ n 2))))
            ((< (/ n 2) 1) 0)
            (else 1))
)

(display (log 16)) (newline)

(define (n-root x n-root)
    ((damp-nth-root n-root (log n-root)) x)
)

(display (n-root 9 2)) (newline)
(display (n-root 64 6)) (newline)
(display (n-root 512 9)) (newline)
(display (n-root 1024 10)) (newline)
(display (n-root 5678 15)) (newline)