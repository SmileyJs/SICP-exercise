(load "../util.scm")
(load "p133_coercion.scm")

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
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (if (equal? type1 type2)
                            (error "No method for these types" (list op type-tags))
                            (let ((t1->t2 (get-coercion type1 type2))
                                (t2->t1 (get-coercion type2 type1)))
                                (cond (t1->t2
                                        (display "t1->t2: ")
                                        (display type1)
                                        (display " ")
                                        (display type2)
                                        (apply-generic op (t1->t2 a1) a2))
                                    (t2->t1
                                        (display "t2->t1: ")
                                        (display type2)
                                        (display " ")
                                        (display type1)
                                        (apply-generic op a1 (t2->t1 a2)))
                                    (else
                                        (error "No method for these types"
                                            (list op type-tags))
                                    )
                                )
                            )
                        )
                    )
                (error "No method for these types" (list op type-tags)))
            )
        )
    )
)

(define (install-scheme-number-package)
    (define (tag x)
        (attach-tag 'scheme-number x))

    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))

    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))

    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))

    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))

    ;(put 'exp '(scheme-number scheme-number)
        ;(lambda (x y) (tag (expt x y))))

    (put 'make 'scheme-number
        (lambda (x) (tag x)))

'done)

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

(install-scheme-number-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n)
)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y)
)

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a)
)

(define (scheme-number->scheme-number n) n)
(define (complex->complex n) n)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

(display (exp (make-scheme-number 2) (make-scheme-number 3)))
