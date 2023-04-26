(load "../util.scm")


(display "3_6")
(newline)

(define (rand-update i)
    (+ i 5)
)

(define rand-init 10)

(define rand
    (let ((x rand-init))
        (define (generate)
            (set! x (rand-update x))
            x
        )

        (define (reset new-value)
            (set! x new-value)
            x
        )

        (define (dispatch m)
            (cond ((eq? m 'generate) generate)
                ((eq? m 'reset) reset)
                (else 
                    error "Invalid method: " m)
            )
        )

        dispatch
    )
)

(display ((rand 'generate)))
(newline)
(display ((rand 'generate)))
(newline)
(display ((rand 'reset) 100))
(newline)
(display ((rand 'generate)))
(newline)
(display ((rand 'generate)))