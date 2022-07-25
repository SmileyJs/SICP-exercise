(load "../util.scm")

(define (element-of-set? x set)
    (cond   ((null? set) #f)
            ((< x (car set)) #f)
            ((= x (car set)) #t)
            (else
                (element-of-set? x (cdr set))))
)

(define (intersection-set s1 s2)
    (if (or (null? s1) (null? s2))
        nil
        (let ((x1 (car s1))
              (x2 (car s2)))
              (cond ((= x1 x2) (cons x1
                                     (intersection-set (cdr s1) (cdr s2))))
                    ((< x1 x2) (intersection-set (cdr s1) s2))
                    ((> x1 x2) (intersection-set s1 (cdr s2))))))
)

(define (adjoin-set x set)
    (define (iter set result)
        (cond   ((null? set)
                    (cons result x))
                ((> x (car set))
                    (iter (cdr set) (cons result (car set))))
                ((< x (car set))
                    (cons (cons result x) set))))
    (iter set nil)
)

(define (adjoin-set-2 x set)
    (cond   ((null? set) (list x))
            ((= x (car set)) set)
            ((< x (car set)) (cons x set))
            (else (cons (car set) (adjoin-set x (cdr set)))))
)

(define (adjoin-set-3 x set)
    (union-set (list x) set)
)

(define (union-set s1 s2)
    (cond   ((null? s1) s2)
            ((null? s2) s1)
            (else (let ((x1 (car s1)) (x2 (car s2)))
                        (cond   ((= x1 x2) (cons x1 (union-set (cdr s1) (cdr s2))))
                                ((< x1 x2) (cons x1 (union-set (cdr s1) s2)))
                                ((> x1 x2) (cons x2 (union-set s1 (cdr s2))))))))
)

; test
(define list-a (list 1 3 5 7))
(define list-b (list 2 4 5 7))

(display (element-of-set? 2 list-a))
(newline)
(display (element-of-set? 3 list-a))
(newline)
(display (intersection-set list-a list-b))
(newline)
(display (union-set list-a list-b))
(newline)
(display (adjoin-set 6 list-a))