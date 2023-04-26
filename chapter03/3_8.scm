(load "../util.scm")

(define f
    (let ((visited #f))
        (lambda (value)
            (if visited
                0
                (begin (set! visited #t)
                    value)
            )
        )
    )
)

(display (+ (f 0) (f 1)))
(newline)
(display (+ (f 1) (f 0)))