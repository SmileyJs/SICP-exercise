; utility method the SICP execrises.

(define (expmod base exp m)
    ;(format #t "base: ~A, exp: ~A, m: ~A ~%" base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m)) m))
          (else
            (remainder (* base (expmod base (- exp 1) m)) m)
          )
    )
)

(define (fermat-test n)
    ;(format #t "fermat-test ~%")
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
    ;(format #t "fast-prime? ~%")
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false))
)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))
)

(define (average a b)
    (/  (+ a b)
        2)
)

(define (expt b n)
    (define (expt-iter base x product)
        (if (= 0 x)
            product
            (if (even? x)
                (expt-iter (square base) (/ x 2) product)
                (expt-iter base (- x 1) (* product base)))))
    
    (expt-iter b n 1)
)

(define nil '())

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high)))
)

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
            (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence))))
)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))
)

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                         (enumerate-tree (cdr tree)))))
)

(define fold-right accumulate)

(define (fold-left op initial seq)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial seq)
)

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)
