(load "../util.scm")

(define (element-of-set? x set)
    (cond   ((null? set) #f)
            ((equal? x (car set)) #t)
            (else (element-of-set? x (cdr set))))
)

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set))
)

(define (intersection-set s1 s2)
    (cond   ((or (null? s1) (null? s2)) '())
            ((element-of-set? (car s1) s2)
                (cons (car s1) (intersection-set (cdr s1) s2)))
            (else (intersection-set (cdr s1) s2)))
)

(define (union-set s1 s2)
    (accumulate adjoin-set s1 s2)
)

; 2_60
(define adjoin-set-2 cons)

(define (intersection-set-2 s1 s2)
    (define (iter set result)
        (cond   ((or (null? set) (null? s2)) result)
                ((and (element-of-set? (car set) s2)
                      (not (element-of-set? (car set) result)))
                        (iter (cdr set) (cons (car set) result)))
                (else (iter (cdr set) result)))
    )
    (iter s1 nil)
)

; test
(define list-a (list 1 3 5 7 1 3))
(define list-b (list 2 4 6 8))

(display (union-set list-a list-b))
(newline)
(define list-c (list 1 3 6 8))
(display (union-set list-a list-c))
(newline)
(display (intersection-set list-a list-c))
(newline)
(display (intersection-set-2 list-a list-c))