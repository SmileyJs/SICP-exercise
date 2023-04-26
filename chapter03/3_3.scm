(load "../util.scm")
(load-option 'format)

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
            "Incorrent password"
        )

        (define (call-police)
             "call-911"
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

(define A (make-account 100 'password))

(display ((A 'withdraw 'password) 50))
(newline)
(display ((A 'deposit 'else) 40))
(newline)
(display ((A 'deposit 'else) 40))
(newline)
(display ((A 'deposit 'else) 40))
(newline)
(display ((A 'deposit 'else) 40))
(newline)
(display ((A 'withdraw 'password) 50))
(newline)
(display ((A 'deposit 'else) 40))
(newline)
(display ((A 'deposit 'else) 40))
(newline)
(display ((A 'deposit 'else) 40))
(newline)
