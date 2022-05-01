(load "../util.scm")

(define (reverse seq)
    (fold-right (lambda (x y)
                        (append y (list x)))
                nil
                seq)
)

(define (reverse-1 seq)
    (fold-left (lambda (x y)
                    (cons y x))
                nil
                seq)
)

(define list-a (list 1 2 3 4 5))

(display (reverse list-a))
(newline)
(display (reverse-1 list-a))