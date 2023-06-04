(load "../util.scm")

(define (make-queue)
    (let ((front-ptr '())
          (rear-ptr '()))   
        
        (define (empty?)
            (null? front-ptr)
        )

        (define (insert! item)
            (let ((new-pair (cons item '())))
                (cond ((empty?)
                        (set! front-ptr new-pair)
                        (set! rear-ptr new-pair))
                      (else
                        (set-cdr! rear-ptr new-pair)
                        (set! rear-ptr new-pair)))
                front-ptr
            )
        )

        (define (delete!)
            (cond ((empty?)
                    (error "DELETE called with empty queue!"))
                  (else
                    (set! front-ptr (cdr front-ptr))))
            front-ptr
        )

        (define (print)
            (define (iter front)
                (if (null? front)
                    (newline)
                    (begin
                        (display (car front))
                        (iter (cdr front))
                    )
                )
            )
            (iter front-ptr)
        )

        (define (test)
            (display "hello")
        )

        (define (dispatch m)
            (cond ((eq? m 'empty?) (empty))
                  ((eq? m 'insert!) insert!)
                  ((eq? m 'delete!) (delete!))
                  ((eq? m 'print) (print))
                  (else error "Invalid operation with queue" m)
            )
        )

        dispatch
    ) 
)

(define (empty-queue? q)
    (q 'empty?)
)

(define (insert-queue! q item)
    ((q 'insert!) item)
)

(define (delete-queue! q)
    (q 'delete!)
)

(define (print-queue q)
    (q 'print)
)

(define q1 (make-queue))
(print-queue q1)

(insert-queue! q1 'a)
(print-queue q1)

(insert-queue! q1 'b)
(print-queue q1)

(insert-queue! q1 'c)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)