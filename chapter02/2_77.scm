(load "../util.scm")
(load "p186_make_table.scm")

(define (attach-tag tag content)
    (cons tag content)
)

(define (type-tag data)
    (cond   ((number? data) 'scheme-number)
            ((pair? data) (car data))
            (else
                (error "bad tagged data -- TYPE-TAG" data)))
)

(define (contents data)
    (cond   ((number? data) data)
            ((pair? data) (cdr data))
            (else
                (error "bad tagged data -- CONTENTS" data)))
)

(define (apply-generic op . args)
    (display "apply-generic: ")
    (display op)
    (display "  ")
    (display args)
    (newline)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these types --APPLY-GENERIC"
                (list op type-tags)))))
)

(define (add-complex v1 v2)
    (make-from-real-imag    (+  (real-part v1) (real-part v2))
                            (+  (imag-part v1) (imag-part v2)))
)

(define (sub-complex v1 v2)
    (make-from-real-imag    (-  (real-part v1)  (real-part v2))
                            (-  (imag-part v1)  (imag-part v2)))
)

(define (mul-complex v1 v2)
    (make-from-mag-ang  (*  (magnitude v1) (magnitude v2))
                        (+  (angle v1)  (angle v2)))
)

(define (div-complex v1 v2)
    (make-from-mag-ang  (/  (magnitude v1) (magnitude v2))
                        (-  (angle v1) (angle v2)))
)

;rectangular
(define (install-rectangular-package)
    (define (real-part z)
        (car z)
    )

    (define (imag-part z)
        (cdr z)
    )

    (define (magnitude z)
        (sqrt (+ (square (real-part z)) (square (imag-part z))))
    )

    (define (angle z)
        (atan (imag-part z) (real-part z))
    )

    (define (make-from-real-imag x y)
        (cons x y)
    )

    (define (make-from-mag-ang r a)
        (cons (* r (cos a) (* r (sin a))))
    )

    (define (tag x) (attach-tag 'rectangular x))

    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
        (lambda (x y) (tag (make-from-mag-ang x y))))
'done)

;polar
(define (install-polar-package)
    (define (real-part z)
        (* (magnitude z) (cos (angle z)))
    )

    (define (imag-part z)
        (* (magnitude z) (sin (angle z)))
    )

    (define (magnitude z)
        (car z)
    )

    (define (angle z)
        (cdr z)
    )

    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
            (atan y x))
    )

    (define (make-from-mag-ang r a)
        (cons r a)
    )

    (define (tag x) (attach-tag 'polar x))

    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
        (lambda (x y) (tag (make-from-mag-ang x y))))
'done)

(define (install-scheme-number-package)
    (define (tag x)
        (attach-tag 'scheme-number x)
    )
    
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y)))
    )

    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y)))
    )

    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y)))
    )

    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y)))
    )

    (put 'make 'scheme-number
        (lambda (x) (tag x))
    )

    (put 'equ? '(scheme-number scheme-number)
        (lambda (x y) (= x y))
    )

    (put '=zero? '(scheme-number)
        (lambda (value)
            (= value 0)))
'done)

(define (install-rational-package)
    (define (numer x)
        (car x)
    )

    (define (denom x)
        (cdr x)
    )

    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g)))
    )

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
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer x)))
    )

    (define (tag x)
        (attach-tag 'rational x)
    )

    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y)))
    )

    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y)))
    )

    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y)))
    )

    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y)))
    )

    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d)))
    )

    (define (equ? x y)
        (and (= (numer x) (numer y))
             (= (denom x) (denom y)))
    )

    (put 'equ? '(rational rational) equ?)

    (put '=zero? '(rational)
        (lambda (x) (= 0 (numer x)))
    )

'done)

(define (install-complex-package)
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y)
    )

    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a)
    )

    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2)))
    )

    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2)))
    )

    (define (mul-complex z1 z2)
        (make-from-mag-ang  (* (magnitude z1) (magnitude z2))
                            (+ (angle z1) (angle z2)))
    )

    (define (div-complex z1 z2)
        (make-from-mag-ang  (/ (magnitude z1) (magnitude z2))
                            (- (angle z1) (angle z2)))
    )

    (define (tag z) (attach-tag 'complex z))

    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))

    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))

    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))

    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))

    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y)))
    )

    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a)))
    )

    (define (magnitude z)
        ((get 'magnitude '(rectangular)) z)
    )

    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)

    (put 'equ? '(complex complex)
        (lambda (x y) (and (= (real-part x) (real-part y))
                           (= (imag-part x) (imag-part y))))
    )

    (put '=zero? '(complex)
        (lambda (x) (and (= (real-part x) 0)
                         (= (imag-part x) 0)))
    )

'done)

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)



(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y)
)

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a)
)

(display "================")

(define (make-from-real-image-rectangular r a)
    ((get 'make-from-real-imag 'rectangular) r a)
)

(define (make-rat x y)
    ((get 'make 'rational) x y)
)

(define (magnitude z)
    (apply-generic 'magnitude z)
)

(display (magnitude (make-from-real-image-rectangular 3 4)))
(newline)
;(display (magnitude (make-complex-from-real-imag 3 4)))

(define (make-scheme-number x)
    ((get 'make 'scheme-number) x)
)

(display (add (make-scheme-number 5) (make-scheme-number 10)))
(newline)
(display (add 5 6))

(define (equ? x y)
    (apply-generic 'equ? x y)
)

(define (=zero? x)
    (apply-generic '=zero? x)
)

;2-39
(display (equ? 2 3))
(newline)

(display (equ? (make-rat 1 2) (make-rat 2 4)))
(newline)
(display (equ? (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4)))
(newline)
(display (=zero? (make-scheme-number 5)))
(newline)
(display (=zero? (make-rat 0 9)))
(newline)
(display (=zero? (make-complex-from-real-imag 3 4)))
(newline)
(display (=zero? (make-complex-from-real-imag 0 0)))