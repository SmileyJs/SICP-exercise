(load "../util.scm")
(load "3_13.scm")

'3_18
(define (isCircle l)
    (let ((seen '()))
        (define (helper crt)
            (if (pair? crt)
                (if (false? (memq (car crt) seen))
                    (begin
                        (set! seen (cons (car crt) seen))
                        (helper (cdr crt))
                    )
                    true
                )
                false
            )
        )
        (helper l)
    )
)

'3_18
(define (isCircle2 l)
    (define (helper crt seen)
        (if (pair? crt)
            (if (false? (memq (car crt) seen))
                (helper (cdr crt) (cons (car crt) seen))
                true
            )
            false
        )
    )
    (helper l nil)
)

'3_19
(define (isCircle3 l)
    (define (helper slow fast)
        (cond ((eq? fast slow)
              true)
              ((or (null? (cdr fast)) (null? (cddr fast)))
              false)
              (else
                  (helper (cdr slow) (cddr fast))
              )
        )
    )

    (helper l (cdr l))
)

(display (isCircle2 z))

(define l (list 1 2 3 4 5))

(newline)
(display (isCircle2 l))

(define (display-list l)
    (define (helper l)
        (if (pair? l)
            (begin
                (display (car l))
                (display " ")
                (helper (cdr l))
            )
            nil     
        ))

    (helper l)
)

(define l1 (list 1 2))

(newline)
(display (isCircle3 l))
(newline)
(display (isCircle3 l1))
(newline)
(display (isCircle3 z))