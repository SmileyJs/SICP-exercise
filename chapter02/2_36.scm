(load "../util.scm")

(define (car-n seqs)
    (map car seqs)
)

(define (cdr-n seqs)
    (map cdr seqs)
)

(define (accumulate-n op init seqs)
    (if (null? (car seqs)) 
        nil
        (cons (accumulate op init (car-n seqs))
              (accumulate-n op init (cdr-n seqs))))
)

(define list-a (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(display list-a)
(newline)
(display (car-n list-a))
(newline)
(display (cdr-n list-a))
(newline)
(display (cdr-n (cdr-n list-a)))
(newline)
(display (cdr-n (cdr-n (cdr-n list-a))))
(newline)
(display (accumulate-n + 0 list-a))