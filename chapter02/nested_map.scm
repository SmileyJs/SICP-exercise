(load "../util.scm")

(define (prime-sum? pair)
    (fast-prime? (+ (car pair) (cadr pair)) 3)
)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define (unique-pairs n)
    (flatmap (lambda (i)
                (map (lambda (j) (list i j))
                    (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n))
)

(define (prime-sum-pair n)
    (map make-pair-sum (filter prime-sum? (unique-pairs n)))
)

(display (prime-sum-pair 6))
(newline)
(display (unique-pairs 5))