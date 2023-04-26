(load "../util.scm")

(define (count-pairs x)
    (define (count-helper x seen)
        (if (and (pair? x)
                 (false? (memq x seen)))
            (count-helper 
                (car x)
                (count-helper (cdr x) (cons x seen)))
            seen
        )
    )

    (length (count-helper x '()))
)


(define x (cons 1 2))
(define y (cons x x))
(define z (cons y y))
(define u (cons 2 3))
(define v (cons z u))

(display (count-pairs v))