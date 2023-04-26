(load "../util.scm")

(display "3_7")
(newline)

(define (make-account balance passwd)
    (let ((max-try-times 7)
          (try-times 0))

        (define (withdraw amount)
            (if (>= balance amount)
                (begin  (set! balance (- balance amount))
                        balance)
                "Insufficient founds!"))

        (define (deposit amount)
            (begin  (set! balance (+ balance amount))
                    balance))

        (define (passed-match? given-passwd)
            (eq? given-passwd passwd)
        )

        (define (display-invalid-passed args)
            (display "Incorrect password"))

        (define (call-police)
            (display "call-911")
        )

        (define (dispatch m pwd)
            (if (passed-match? pwd)
                (begin
                    (set! try-times 0)
                    (cond   ((eq? m 'withdraw) withdraw)
                            ((eq? m 'deposit) deposit)
                            (else (error "Unknown request -- make-account:" m))))
                (begin
                    (set! try-times (+ try-times 1))
                    (if (>= try-times max-try-times)
                        (call-police)
                        display-invalid-passed))))

        dispatch
    )
)

(define (make-joint ori-acc ori-pw new-pw)
    (lambda (given-pw mode)
        (if (eq? given-pw new-pw)
            (ori-acc mode ori-pw)
            (display "incorrect joint password")
        )
    )
)

(define jack (make-account 1000 'jack-pw))

(define ross (make-joint jack 'jack-pw 'ross-pw))

(display ((jack 'deposit 'jack-pw) 100))
(newline)
(display ((jack 'withdraw 'jack-pw) 50))
(newline)
(display ((ross 'ross-pw 'withdraw) 500))
(newline)
(display ((ross 'ross-pw 'deposit) 400))
(newline)
(display ((ross 'jack-pw 'withdraw) 100))