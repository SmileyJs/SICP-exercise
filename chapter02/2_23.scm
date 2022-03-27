(load "../util.scm")

(define (for-each func items)
    (define (iter rest)
        (if (not (null? rest))
            (begin
                (func (car rest))
                (iter (cdr rest)))
        )
    )
    (iter items)
)

(define (print x)
    (newline)
    (display x)
)

(for-each print (list 1 3 5 6 7))