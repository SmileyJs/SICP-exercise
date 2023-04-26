(load "../util.scm")

(display "3_13")

(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x)))
)

(define (append x y)
    (if (null? x)
        y
        (cons (car x) (append (cdr x) y)))
)

(define (append! x y)
    (set-cdr! (last-pair x) y)
    x
)


(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

(display z)
(newline)
(display (cdr x))
(newline)

(define w (append! x y))

(display w)
(newline)
(display (cdr x))
(newline)

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x
)

(define z (make-cycle (list 'a 'b 'c)))

'(display z) 