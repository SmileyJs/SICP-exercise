(load "../util.scm")

(define (make-deque)
    (cons '() '())
)

(define (front-deque q)
    (car q)
)

(define (rear-deque q)
    (cdr q)
)

(define (empty-deque? q)
    (null? (front-deque q))
)

(define (rear-insert-deque! q item)
    (let ((new-pair (cons item '())))
        (cond ((empty-deque?  q)
                (set-car! q new-pair)
                (set-cdr! q new-pair)
              q)
              (else
                (set-cdr! (rear-deque q) new-pair)
                (set-cdr! q new-pair)
                q)))
)

(define (front-insert-deque! q item)
    (cond ((empty-deque? q)
                (rear-insert-deque! q item))
          (else
            (set-car! q (cons item (front-deque q)))
            q)))

(define (front-delete-deque! q)
    (if (empty-deque? q)
        (error "DELETE called with empty deque!")
        (set-car! q (cdr (front-deque q)))
    )
)

(define (rear-delete-deque! q)
    (define (iter deque lst)
        (cond ((null? (cddr lst))
                (set-cdr! lst '())
                (set-cdr! deque lst)
                deque)
              (else
                (iter deque (cdr lst)))))

    (cond ((empty-deque? q)
            (error "REAR DELETE called with empty deque!"))
          ((null? (cdr (front-deque q)))
            (set-car! q '())
            q)
          (else
            (iter q (car q))))
)

(define (print-deque q)
    (define (iter front-ptr)
        (if (null? front-ptr)
            (newline)
            (begin (display (car front-ptr))
                (iter (cdr front-ptr))
            )
        )
    )

    (iter (car q))
)

(define q1 (make-deque))

(display (empty-deque? q1))
(newline)

(rear-insert-deque! q1 'a)
(print-deque q1)

(rear-insert-deque! q1 'b)
(rear-insert-deque! q1 'c)
(print-deque q1)

(front-delete-deque! q1)
(print-deque q1)

(front-insert-deque! q1 'd)
(print-deque q1)

(rear-delete-deque! q1)
(print-deque q1)