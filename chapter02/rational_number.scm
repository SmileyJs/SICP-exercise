(load-option 'format)
(load "../util.scm")

(define (make-rat n d)
    ;(format #t "~%n: ~A, d: ~A~%" n d)
    (let ((g (abs (gcd n d))))
        (if (< d 0)
            (cons (/ (- n) g) (/ (- d) g))
            (cons (/ n g) (/ d g))))
)

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))
)

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))
)

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y)))
)

(define (div-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y)))
)

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y)))
)

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))
)



; test
(define one-half (make-rat 1 (- 2)))

(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))