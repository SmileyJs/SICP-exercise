(load "../util.scm")

(define (count-leaves tree)
    (accumulate + 0 (map (lambda (x) 1) (enumerate-tree tree)))
)

(define (count-leaves-without-enum tree)
    (accumulate + 0 (map (lambda (t)
                            (cond ((null? t) 0)
                                  ((not (pair? t)) 1)
                                  (else (count-leaves-without-enum t))))
                         tree))
)

(define tree-a (list 1 (list 2 (list 3 4)) 5))

(display (count-leaves tree-a))
(newline)
(display (count-leaves-without-enum tree-a))