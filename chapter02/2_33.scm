(load "../util.scm")

(define (map-self p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) 
                nil
                sequence)
)

(define (append-self seq1 seq2)
    (accumulate cons seq2 seq1)
)

(define (length-self sequence)
    (accumulate (lambda (x y) (+ 1 y))
                0
                sequence)
)

(define list-a (list 1 3 5 7 9))
(define list-b (list 2 4 6 8 10))

(display (map-self (lambda (x) (+ x 3)) list-a))
(newline)
(display (append-self list-a list-b))
(newline)
(display (append list-a list-b))
(newline)
(display (length-self list-a))