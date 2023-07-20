(load "../util.scm")
(load "3_25.scm")

(display "3_27 \n")


(define (fib n)
    (display "calculate: ")
    (display n)
    (newline)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))    
    )
)

(define (memoize f)
    (let ((table (make-table equal?)))
        (lambda (x)
            (let ((prev-result ((table 'lookup-proc) x)))
                (or prev-result
                    (let ((result (f x)))
                        ((table 'insert-proc!) x result)
                        result
                    )
                )
            )
        )
    )
)

(define mem-fib
    (memoize (lambda (n)
                (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else (+ (fib (- n 1))
                            (fib (- n 2))))    
                )
             )
    )
)

(display (fib 5))
(newline)
(display (fib 2))
(newline)
(display (fib 9))
(newline)
(mem-fib 5)
(mem-fib 2)
(mem-fib 9)
(display (mem-fib 5))
(newline)
(display (mem-fib 2))
(newline)
(display (mem-fib 9))