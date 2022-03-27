(load "../util.scm")

(define (square-list items)
    ;(if (null? items)
    ;    nil
    ;    (cons (square (car items)) (square-list (cdr items))))

    (define (iter rest result)
        (if (null? rest)
            result
            (iter   (cdr rest)
                    (cons   result
                            (square (car rest))))))

    (iter items nil)
)

(define (square-list-map items)
    (map square items)
)

(define test-data (list 1 3 5 6 9))
(newline)
(display (square-list test-data))
(newline)
(display (square-list-map test-data))