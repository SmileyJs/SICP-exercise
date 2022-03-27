(load "../util.scm")
(load "2_17.scm")

(define list-a (list (list 1 2 3) (list 4 (list 7 8 9))))

(define (deep-list-reverse list)
    (define (iter items result)
        (if (null? items)
            result
            (iter (cdr items) 
                  (cons (if (pair? (car items))
                            (deep-list-reverse (car items))
                            (car items))
                        result))
        )
    )

    (iter list nil)
)

(display list-a)
(newline)
(display (list-reverse list-a))
(newline)
(display (deep-list-reverse list-a))
