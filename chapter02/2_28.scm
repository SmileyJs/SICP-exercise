(load "../util.scm")

(define (fringe items)
    (display items)
    (newline)
    (cond ((null? items) nil)
          ((not (pair? items)) (list items))
          (else (append (fringe (car items))
                        (fringe (cdr items)))))
)

(define list-x (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define list-y (list (list 1 2 3) (list 4 (list 7 8 9))))

(display (fringe list-x))
(newline)
(display (fringe list-y))
(newline)
(display (fringe (list list-x list-x)))