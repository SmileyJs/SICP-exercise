; 1_29.scm

(load-option 'format)

(define (cube n)
    (* n n n)
)

(define (inc n)
    (+ n 1)
)

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b)))
)

; 1-30
(define (sum-iter team a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (team a)))
        )
    )

    (iter a 0)
)

(define (sum-integers a b)
    (define (identity x) x)
    (sum-iter identity a inc b)
)

(define (sum-cubes a b)
    (sum cube a inc b)
)

(define (pi-sum a b)
    (define (pi-term n)
        (/ 1.0 (* n (+ n 2))))
    (define (pi-next n)
        (+ n 4)
    )
    (sum pi-term a pi-next b)
)

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))

    (* (sum f (+ a (/ dx 2)) add-dx b) dx)
)

(display (sum-integers 0 10))
(newline)
(display (sum-cubes 0 10))
(newline)
(display (* 8 (pi-sum 1 1000)))
(newline)
(display (integral cube 0 1 0.01))
(newline)
(display (integral cube 0 1 0.001))
(newline)

; simpson

(define (simpson f a b n)
    (define h (/ (- b a) n))
    (define (y k) (f (+ a (* k h))))
    (define (factor index)
        (cond ((or (= index 0) (= index n)) 1)
              ((even? index) 2)
              (else 4)))
    (define (term k) (* (factor k) (y k)))
    (define (next k) (+ k 1))

    (* (/ h 3) (sum-iter term (exact->inexact 0) next n))
)

(display (simpson cube 0 1 100))
(newline)
(display (simpson cube 0 1 1000))

