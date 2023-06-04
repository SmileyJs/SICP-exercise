(load "../util.scm")

(define (cons x y)
    (define (set-x! v) (set! x v))
    (define (set-y! v) (set! y v))

    (define (dispatch m)
        (cond   ((eq? m 'car) x)
                ((eq? m 'cdr) y)
                ((eq? m 'set-car!) set-x!)
                ((eq? m 'set-cdr!) set-y!)
                (else (error "Undefined operation -- CONS" m))))

    dispatch
)

(define (car z) (z 'car))

(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
    ((z 'set-car!) new-value)
    z
)

(define (set-cdr! z new-value)
    ((z 'set-cdr!) new-value)
    z
)

; 3_20
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)

(display (car x))
(newline)

(define (front-ptr q)
    (car q)
)

(define (rear-ptr q)
    (cdr q)
)

(define (set-front-ptr! q item)
    (set-car! q item)
)

(define (set-rear-ptr! q item)
    (set-cdr! q item)
)

(define (make-queue)
    (cons '() '())
)

(define (empty-queue? q)
    (null? (front-ptr q))
)

(define (front-queue q)
    (if (empty-queue? q)
        (error "front called with empty queue!")
        (car q))
)

(define (insert-queue! q item)
    (let ((new-pair (cons item '())))
        (cond  ((empty-queue? q)
                (set-front-ptr! q new-pair)
                (set-rear-ptr! q new-pair)
                q)
               (else
                (set-cdr! (rear-ptr q) new-pair)
                (set-rear-ptr! q new-pair)
                q)))
)

(define (delete-queue! q)
    (if (empty-queue? q)
        (error "DELETE called with empty queue!")
        (set-front-ptr! q (cdr (front-ptr q)))
    )
)

; 3_21
(define q1 (make-queue))
(insert-queue! q1 'a)
(display q1)
(newline)

(insert-queue! q1 'b)
(display q1)
(newline)

(delete-queue! q1)
(display q1)
(newline)

(delete-queue! q1)
(display q1)