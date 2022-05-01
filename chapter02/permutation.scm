(load "../util.scm")

(define (permutation seq)
    (if (null? seq)
        (list nil)
        (flatmap (lambda (x) 
                    (map (lambda (y) (cons x y))
                        (permutation (remove x seq))))
                seq)
    )
)

(define (remove item seq)
    (filter (lambda (x) (not (= item x)))
            seq)
)

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define list-a (list 1 2 3 4))

(display list-a)
(newline)
(display (permutation list-a))
(newline)
(display (cons 1 nil))
(newline)
(display (cons 1 (list nil)))