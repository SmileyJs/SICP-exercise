(load "../util.scm")

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1)))
)

(define (list-length items)
    (define (iter-helper a count)
        (if (null? a)
            count
            (iter-helper (cdr a) (+ count 1))))

    (iter-helper items 0)
)

(define (list-append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (list-append (cdr list1) list2)))
)

(define (list-pair items)
    (if (null? (cdr items))
        items
        (list-pair (cdr items)))
)

(define (list-reverse items)
    (define (helper a result)
        (if (null? a)
            result
            (helper (cdr a) (cons (car a) result))))
    (helper items ())
)

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7 9))

(display (list-ref squares 2))
(newline)
(display (list-length squares))
(newline)
(display (list-append squares odds))
(newline)
(display (list-pair squares))
(newline)
(display (list-pair odds))
(newline)
(display (list-reverse squares))