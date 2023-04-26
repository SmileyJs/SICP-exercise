(load "../util.scm")

(display "restart")
(newline)

(define (make-withdraw balance)
    (lambda (amount)
        (if (<= amount balance)
            (begin (set! balance (- balance amount))
                balance)
            ("insuffcient funds!")
        )
    )
)

(define (make-account balance passwd)
    (let ((max-attemp-count 7)
         (tried-count 0))
    
        (define (withdraw amount)
            (if (<= amount balance)
                (begin (set! balance (- balance amount))
                    balance)
                "insuffcient funds!"))

        (define (deposit amount)
            (begin (set! balance (+ balance amount))
                balance))

        (define (passwd-match? arg)
            (eq? arg passwd)
        )

        (define (dispatch m wd)
            (if (passwd-match? wd)
                (begin  (set! tried-count 0)
                        (cond ((eq? m 'withdraw) withdraw)
                            ((eq? m 'deposit) deposit)
                            (else (error "Unknown request-make account" m))))
                (begin (set! tried-count (+ 1 tried-count))
                        (if (>= tried-count max-attemp-count)
                            (lambda (x) "call-911")
                            (lambda (x) "Incorrent password")))
            )
        )

        dispatch
    )
)

; 3-1
(define (make-accumulator amount)
    (lambda (n)
        (begin (set! amount (+ n amount))
            amount))
)

(define A (make-accumulator 14))

; 3-2
(define (make-monitored f)
    (let ((count 0))
        (lambda (m)
            (cond ((eq? m 'how-many-calls?) count)
                  ((eq? m 'reset-count) 
                        (set! count 0))
                  (else (begin (set! count (+ 1 count))
                            (f m))))))
)

(define sqrt-x (make-monitored sqrt))

(define A (make-account 1000 'secret))

(define (rand-update arg)
    (+ arg 5)
)

(define random-init 100)

(define rand
    (let ((x random-init))
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
                    (else error "invalid method: " m))
        )

        dispatch
    )
)

(define (monte-carlo trials experiment)
    (define (iter remaining passed)
        (cond   ((= 0 remaining) (/ passed trials))
                ((experiment) (iter (- remaining 1) (+ 1 passed)))
                (else (iter (- remaining 1) passed))))

    (iter trials 0)
)

(define (cesaro-test)
    (= (gcd (rand) (rand)) 1)
)

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range)))
)

(define (estimate-integral func x1 x2 y1 y2 trials)
    (* 4 (monte-carlo trials (lambda ()
        (func (random-in-range x1 x2) (random-in-range y1 y2)))))
)

(define (estimate-pi trials)
    (exact->inexact 
        (estimate-integral 
            (lambda (x y) (< (+ (square x) (square y)) 1.0))
            -1.0 1.0 -1.0 1.0 trials))
)

; 3-7
(define (make-joint account ori-passwd new-passwd)
    (lambda (ope pwd)
        (if (eq? pwd new-passwd)
            (account ope ori-passwd)
            (display "incorrect joint passwd!\n")
        )
    )
)

(define A (make-account 100 'secret))

(display ((A 'withdraw 'secret) 30))
(newline)

(define B (make-joint A 'secret 'new-pwd))

(display ((B 'withdraw 'new-pwd ) 10))
(newline)

; 3-8
(define func-order
    (let ((visited #f))
        (lambda (value)
            (if visited
                0
                (begin (set! visited #t)
                 value)
            )
        )
    )
)

(display (+ (func-order 0) (func-order 1)))
(newline)
(display (+ (func-order 1) (func-order 0)))

(newline)
(display "end")