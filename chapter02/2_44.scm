(load "../util.scm")

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (besides smaller smaller))))
)

(define (split conbine-1 conbine-2)
    (define (inner painter n)
        (if (= n 0)
            painter
            (let ((smaller (inner painter (- n 1)))
                (conbine-1 pointer (congine-2 smaller smaller))))))

    (inner)
)

(define up-split (split below besides))

(define right-split (split besides below))