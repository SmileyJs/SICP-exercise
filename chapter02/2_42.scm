(load "../util.scm")

(define (cons-test a b)
    (cons a b)
)

(define (append-test a b)
    (append a b)
)

(display (cons-test (list 1 2) (list 1 2)))
(newline)
(display (append-test (list 1 2) (list 1 2)))
(newline)

(define empty-board nil)

(define (make-coordinate x y)
    (list x y)
)

(define (coordinate-x pair)
    (car pair)
)

(define (coordinate-y pair)
    (cadr pair)
)

(define (safe? k positions)
    (define (self-pairs? lhs rhs)
        (not (or (= (coordinate-x lhs) (coordinate-x rhs))
                (= (coordinate-y lhs) (coordinate-y rhs))
                (= (abs (- (coordinate-x lhs) (coordinate-x rhs)))
                    (abs (- (coordinate-y lhs) (coordinate-y rhs)))))))

    (define (safe-iter crt rest)
        (if (null? rest)
            #t
            (and (self-pairs? crt (car rest))
                (safe-iter crt (cdr rest)))))

    (safe-iter (car positions) (cdr positions))
)

(define (adjoin-position new-row k rest-queens)
    (cons (make-coordinate k new-row)  rest-queens )
)

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter 
                (lambda (positions) (safe? k positions))
                (flatmap 
                    (lambda (rest-queens)
                        (map (lambda (new-row)
                                (adjoin-position new-row k rest-queens))
                            (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))

    (queen-cols board-size)
)

(let ((result (queens 8)))
    (for-each (lambda (pos)
                (begin 
                    (display pos)
                    (newline)))
            result)
    (display (length result))
)