(load "../util.scm")

(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest
                    (map (lambda (r) (cons (car s) r))
                         rest))))
)

(define la (list 1 2 3))

(display (subsets la))